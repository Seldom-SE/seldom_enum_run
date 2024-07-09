use std::{convert::Infallible, marker::PhantomData};

use anyhow::anyhow;
use bevy::{ecs::world::Command, prelude::*};
use serde::{Deserialize, Serialize};

pub use anyhow::Result;

pub struct EnumRunPlugin<R: EnumRun>(PhantomData<R>);

impl<R: EnumRun> Default for EnumRunPlugin<R> {
    fn default() -> Self {
        Self(PhantomData)
    }
}

impl<R: EnumRun> Plugin for EnumRunPlugin<R> {
    fn build(&self, app: &mut App) {
        let systems = R::systems(app.world_mut());
        app.insert_resource(systems);
    }
}

pub trait EnumRun: Serialize + for<'a> Deserialize<'a> + Clone + TypePath + Send + Sync {
    type Systems: Resource + Clone;
    type Given: Send;
    type Output;

    fn systems(world: &mut World) -> Self::Systems;
    fn run_inner(
        self,
        given: Self::Given,
        systems: Self::Systems,
        world: &mut World,
    ) -> Result<Self::Output>;

    fn run(self, given: Self::Given, world: &mut World) -> Result<Self::Output> {
        let Some(systems) = world.get_resource::<Self::Systems>() else {
            return Err(anyhow!(
                "`EnumRun::Systems` missing when running `EnumRun`. \
                Make sure `EnumRunPlugin` is added."
            ));
        };

        self.run_inner(given, systems.clone(), world)
    }
}

#[derive(Resource, Clone)]
pub struct NoSystemsSystems;

#[derive(Serialize, Deserialize, Clone, TypePath)]
pub enum NoSystems<G: Clone + Send + Sync + TypePath = ()> {
    #[serde(skip)]
    _G(PhantomData<G>),
}

impl<G: Clone + Send + Sync + TypePath> EnumRun for NoSystems<G> {
    type Systems = NoSystemsSystems;
    type Given = G;
    type Output = Infallible;

    fn systems(_: &mut World) -> Self::Systems {
        NoSystemsSystems
    }

    fn run_inner(
        self,
        _: Self::Given,
        NoSystemsSystems: Self::Systems,
        _: &mut World,
    ) -> Result<Infallible> {
        Err(anyhow!("attempted to run a system for `NoSystems`"))
    }
}

pub struct RunByEnum<R: EnumRun> {
    given: R::Given,
    run: R,
}

impl<R: EnumRun> RunByEnum<R> {
    pub fn given(given: R::Given, run: R) -> Self {
        Self { given, run }
    }
}

impl<R: EnumRun<Given = ()>> RunByEnum<R> {
    pub fn new(run: R) -> Self {
        Self { given: (), run }
    }
}

impl<R: EnumRun> Command for RunByEnum<R> {
    fn apply(self, world: &mut World) {
        if let Err(err) = self.run.run(self.given, world) {
            error!("error when running `EnumRun`: {err}");
        }
    }
}
