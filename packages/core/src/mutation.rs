use async_trait::async_trait;
use leptos::*;

use crate::utils::*;

#[async_trait(?Send)]
pub trait Mutation: 'static {
    type Input: Clone + 'static;
    type Output: Clone + 'static;
    type Err: Clone + 'static;
    type Result = Result<Self::Output, Self::Err>;

    async fn exec(&self, input: Self::Input) -> Result<Self::Output, Self::Err>;
}

pub type MutationActionT<M: Mutation + ?Sized> = Action<M::Input, Result<M::Output, M::Err>>;
pub type EventAction<E> = Action<E, ()>;

// #[derive(Clone)]
pub struct MutationAction<M: Mutation + ?Sized>(MutationActionT<M>);

impl<M: Mutation + ?Sized> Clone for MutationAction<M> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
impl<M: Mutation + ?Sized> Copy for MutationAction<M> {}

impl<M: Mutation + ?Sized> MutationAction<M> {
    pub fn create<E: From<M::Output> + 'static>(
        cmd: &Shared<M>,
        event_subscribers: &[EventAction<E>],
    ) -> Self {
        let cmd = store_value(cmd.clone());
        let event_subscribers = store_value(event_subscribers.to_vec());
        Self(create_action(move |input: &M::Input| {
            let input = input.clone();
            async move {
                let result = cmd.get_value().exec(input).await;
                if let Ok(r) = result.as_ref() {
                    event_subscribers
                        .get_value()
                        .iter()
                        .for_each(|event_bus| event_bus.dispatch(E::from(r.clone())))
                }
                result
            }
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

pub trait UseMutation<Env, E>: Clone + Sized + 'static {
    fn create(env: Shared<Env>, event_subscribers: &[EventAction<E>]) -> Self;

    fn provide(env: Shared<Env>, event_subscribers: &[EventAction<E>]) -> Self {
        let s = Self::create(env, event_subscribers);
        provide_context(s.clone());
        s
    }

    fn expect() -> Self {
        expect_context()
    }
}
