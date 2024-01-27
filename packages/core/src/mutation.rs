use async_trait::async_trait;
use leptos::*;

use crate::utils::*;

#[cfg_attr(target_arch = "wasm32", async_trait(?Send))]
#[cfg_attr(not(target_arch = "wasm32"), async_trait)]
pub trait Mutation: 'static {
    type Input: Clone + 'static;
    type Output: Clone + 'static;
    type Err: Clone + 'static;
    type Result = Result<Self::Output, Self::Err>;

    async fn exec(&self, input: Self::Input) -> Result<Self::Output, Self::Err>;
}

pub type MutationActionT<M: Mutation + ?Sized> = Action<M::Input, Result<M::Output, M::Err>>;

// #[derive(Clone)]
pub struct MutationAction<M: Mutation + ?Sized>(MutationActionT<M>);

impl<M: Mutation + ?Sized> Clone for MutationAction<M> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
impl<M: Mutation + ?Sized> Copy for MutationAction<M> {}

impl<M: Mutation + ?Sized> MutationAction<M> {
    pub fn new(cmd: &Shared<M>) -> Self {
        let cmd = store_value(cmd.clone());
        Self(create_action(move |input: &M::Input| {
            let input = input.clone();
            async move { cmd.get_value().exec(input).await }
        }))
    }

    pub fn output(&self) -> Signal<Option<M::Output>> {
        let action = self.0;
        Signal::derive(move || (action.value().get().and_then(move |v| v.ok())).map(|v| v.clone()))
    }

    pub fn watch_ok<F>(&self, immediate: bool, callback: F)
    where
        F: Fn(&M::Output) + Clone + 'static,
    {
        let _ = watch(
            self.0.value(),
            move |result, _, _| {
                if let Some(Ok(result)) = result.as_ref() {
                    callback(result);
                }
            },
            immediate,
        );
    }
}

impl<M: Mutation + ?Sized> std::ops::Deref for MutationAction<M> {
    type Target = MutationActionT<M>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub trait UseMutation<Env>: Clone + Sized + 'static {
    fn new(env: Shared<Env>) -> Self;

    fn provide(env: Shared<Env>) -> Self {
        let s = Self::new(env);
        provide_context(s.clone());
        s
    }

    fn expect() -> Self {
        expect_context()
    }
}
