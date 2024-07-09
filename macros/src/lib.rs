use itertools::MultiUnzip;
use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse, parse2, AttrStyle, Data, DeriveInput, Error, Expr, Fields, Index, Meta, Result, Token,
    Type,
};

enum GivenTy<'a> {
    Tuple(Box<[&'a Type]>),
    Ty(&'a Type),
}

impl<'a> GivenTy<'a> {
    fn iter(&self) -> GivenTyIter {
        GivenTyIter {
            given: self,
            index: 0,
        }
    }
}

struct GivenTyIter<'a: 'b, 'b> {
    given: &'b GivenTy<'a>,
    index: u16,
}

impl<'a: 'b, 'b> Iterator for GivenTyIter<'a, 'b> {
    type Item = &'a Type;

    fn next(&mut self) -> Option<Self::Item> {
        let next = match self.given {
            GivenTy::Tuple(tuple) => tuple.get(self.index as usize).copied(),
            GivenTy::Ty(&ref ty) => {
                if self.index == 0 {
                    Some(ty)
                } else {
                    None
                }
            }
        };

        self.index += 1;

        next
    }
}

fn tys(ty: &Type) -> GivenTy {
    match ty {
        Type::Group(group) => tys(&*group.elem),
        Type::Paren(paren) => tys(&*paren.elem),
        Type::Tuple(tuple) => GivenTy::Tuple(tuple.elems.iter().collect()),
        ty => GivenTy::Ty(ty),
    }
}

fn inner(input: TokenStream) -> Result<TokenStream> {
    let input = parse::<DeriveInput>(input)?;

    let vis = input.vis;
    let ident = input.ident;
    let systems_ident = format_ident!("__{ident}Systems");
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let data_enum = match input.data {
        Data::Struct(data_struct) => {
            return Err(Error::new_spanned(
                data_struct.struct_token,
                "structs are not supported",
            ))
        }
        Data::Enum(data_enum) => data_enum,
        Data::Union(data_union) => {
            return Err(Error::new_spanned(
                data_union.union_token,
                "unions are not supported",
            ))
        }
    };

    let mut given = None;
    let mut output = None;

    for attr in input.attrs {
        if !attr.path().is_ident("enum_run") {
            continue;
        }

        if let AttrStyle::Inner(not) = attr.style {
            return Err(Error::new_spanned(
                not,
                "inner attributes are not supported",
            ));
        }

        attr.parse_nested_meta(
            |meta| match meta.path.require_ident()?.to_string().as_str() {
                "given" => {
                    if given.is_some() {
                        return Err(meta.error("duplicate `given` attribute"));
                    }

                    meta.input.parse::<Token![=]>()?;
                    given = Some(meta.input.parse::<Type>()?);

                    if !meta.input.is_empty() && !meta.input.peek(Token![,]) {
                        return Err(meta.error("unexpected token after given type"));
                    }

                    Ok(())
                }
                "output" => {
                    if output.is_some() {
                        return Err(meta.error("duplicate `output` attribute"));
                    }

                    meta.input.parse::<Token![=]>()?;
                    output = Some(meta.input.parse::<Type>()?);

                    if !meta.input.is_empty() && !meta.input.peek(Token![,]) {
                        return Err(meta.error("unexpected token after output type"));
                    }

                    Ok(())
                }
                ident => Err(meta.error(format!("unknown attribute `{ident}`"))),
            },
        )?;
    }

    let given_ty = match given {
        Some(ref given) => tys(given),
        None => GivenTy::Tuple(Box::new([])),
    };

    let (input_ty, systems, run_cases): (Vec<_>, Vec<_>, Vec<_>) = data_enum
        .variants
        .iter()
        .enumerate()
        .map(|(index, variant)| {
            let variant_ident = &variant.ident;
            let path = quote! { #ident::#variant_ident };
            let (tys, pat, fields) = match variant.fields {
                Fields::Named(ref fields) => {
                    let (tys, fields): (Vec<_>, Vec<_>) = fields
                        .named
                        .iter()
                        .map(|field| (&field.ty, field.ident.clone().unwrap()))
                        .unzip();

                    (tys, quote! { #path { #(#fields,)* } }, fields)
                }
                Fields::Unnamed(ref fields) => {
                    let (tys, fields): (Vec<_>, Vec<_>) = fields
                        .unnamed
                        .iter()
                        .enumerate()
                        .map(|(index, field)| (&field.ty, format_ident!("__field_{index}")))
                        .unzip();

                    (tys, quote! { #path(#(#fields,)*) }, fields)
                }
                Fields::Unit => (Vec::new(), path, Vec::new()),
            };

            let input = given_ty.iter().chain(tys);
            let mut system = None;

            for attr in &variant.attrs {
                if !attr.path().is_ident("enum_run") {
                    continue;
                }

                if system.is_some() {
                    return Err(Error::new_spanned(attr, "duplicate `enum_run` attribute"));
                }

                if let AttrStyle::Inner(not) = attr.style {
                    return Err(Error::new_spanned(
                        not,
                        "inner attributes are not supported",
                    ));
                }

                let Meta::List(list) = &attr.meta else {
                    return Err(Error::new_spanned(&attr.meta, "expected `enum_run(...)`"));
                };

                system = Some(parse2::<Expr>(list.tokens.clone())?);
            }

            let Some(system) = system else {
                return Err(Error::new_spanned(&variant, "missing `enum_run` attribute"));
            };

            let index = Index::from(index);

            let input_accesses = match given_ty {
                GivenTy::Tuple(ref ty) => (0..ty.len())
                    .map(|index| {
                        let index = Index::from(index);
                        quote! { __given.#index }
                    })
                    .collect::<Vec<_>>(),
                GivenTy::Ty(_) => vec![quote! { __given }],
            }
            .into_iter()
            .chain(fields.iter().map(|field| quote! { #field }));

            Ok((
                quote! { (#(#input),*) },
                system,
                quote! { #pat => ::bevy::prelude::World::run_system_with_input(
                    __world,
                    __systems.#index,
                    (#(#input_accesses),*),
                )?, },
            ))
        })
        .collect::<Result<Vec<_>>>()?
        .into_iter()
        .multiunzip();

    let given = if let Some(given) = given {
        quote! { #given }
    } else {
        quote! { () }
    };

    let output = if let Some(output) = output {
        quote! { #output }
    } else {
        quote! { () }
    };

    Ok(quote! {
        #[derive(::std::clone::Clone, ::bevy::prelude::Resource)]
        #vis struct #systems_ident #impl_generics(#(::bevy::ecs::system::SystemId<#input_ty, #output>,)*)
            #where_clause;

        #[automatically_derived]
        impl #impl_generics crate::enum_run::EnumRun for #ident #ty_generics #where_clause {
            type Systems = #systems_ident #impl_generics;
            type Given = #given;
            type Output = #output;

            fn systems(__world: &mut ::bevy::prelude::World) -> Self::Systems {
                #systems_ident(#(::bevy::prelude::World::register_system(__world, #systems),)*)
            }

            fn run_inner(
                self,
                __given: Self::Given,
                __systems: Self::Systems,
                __world: &mut ::bevy::prelude::World,
            ) -> crate::enum_run::Result<Self::Output> {
                let output = match self {
                    #(#run_cases)*
                };

                #[allow(unreachable_code)]
                Ok(output)
            }
        }
    }
    .into())
}

#[proc_macro_derive(EnumRun, attributes(enum_run))]
pub fn derive_enum_run(input: TokenStream) -> TokenStream {
    match inner(input) {
        Ok(out) => out,
        Err(err) => err.to_compile_error().into(),
    }
}
