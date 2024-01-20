use async_trait::async_trait;
use leptos::*;
use portaldi::DI;
use serde::de::DeserializeOwned;

use crate::utils::*;

#[cfg_attr(target_arch = "wasm32", async_trait(?Send))]
#[cfg_attr(not(target_arch = "wasm32"), async_trait)]
pub trait Query: Clone + 'static {
    type Input: Clone + PartialEq + 'static;
    type Output: Clone + serde::Serialize + DeserializeOwned + 'static;
    type Err: Clone + serde::Serialize + DeserializeOwned + 'static;
    type Result = Result<Self::Output, Self::Err>;

    async fn exec(&self, input: Self::Input) -> Result<Self::Output, Self::Err>;
}

#[derive(Clone)]
pub struct QueryFetcher<Q: Query> {
    query: StoredValue<DI<Q>>,
    on_ok: StoredValue<Option<Shared<dyn Fn(Q::Output)>>>,
    on_err: StoredValue<Option<Shared<dyn Fn(Q::Err)>>>,
}

impl<Q: Query> QueryFetcher<Q> {
    pub fn new(query: DI<Q>) -> Self {
        Self {
            query: store_value(query),
            on_ok: store_value(None),
            on_err: store_value(None),
        }
    }
    pub fn with_watch_ok(mut self, callback: impl Fn(Q::Output) + 'static) -> Self {
        self.on_ok = store_value(Some(Shared::new(callback)));
        self
    }
    pub fn with_watch_err(mut self, callback: impl Fn(Q::Err) + 'static) -> Self {
        self.on_err = store_value(Some(Shared::new(callback)));
        self
    }

    pub fn run(&self, input: Q::Input) -> ResourceW<Q> {
        let query = self.query;
        let on_ok = self.on_ok;
        let on_err = self.on_err;
        ResourceW(create_resource(move || input.clone(), {
            move |input| Self::fetch(query, on_ok, on_err, input)
        }))
    }
    pub fn run_with_memo(&self, input: Memo<Q::Input>) -> ResourceW<Q> {
        let query = self.query;
        let on_ok = self.on_ok;
        let on_err = self.on_err;
        ResourceW(create_resource(input, {
            move |input| Self::fetch(query, on_ok, on_err, input)
        }))
    }

    async fn fetch(
        query: StoredValue<Shared<Q>>,
        on_ok: StoredValue<Option<Shared<dyn Fn(Q::Output)>>>,
        on_err: StoredValue<Option<Shared<dyn Fn(Q::Err)>>>,
        input: Q::Input,
    ) -> Result<Q::Output, Q::Err> {
        // TODO debounce, cache, retry handling
        let result = query.get_value().exec(input).await;
        match &result {
            Ok(value) => {
                if let Some(f) = on_ok.get_value() {
                    f(value.clone())
                }
            }
            Err(err) => {
                if let Some(f) = on_err.get_value() {
                    f(err.clone())
                }
            }
        }
        result
    }
}

#[derive(Clone, Copy)]
pub struct ResourceW<Q: Query>(Resource<Q::Input, Result<Q::Output, Q::Err>>);

impl<Q: Query> ResourceW<Q> {
    pub fn output(&self) -> Signal<Option<Q::Output>> {
        let r = self.0;
        Signal::derive(move || (r.get().and_then(move |v| v.ok())))
    }

    pub fn output_memo(&self) -> Memo<Option<Q::Output>>
    where
        Q::Output: PartialEq,
    {
        let r = self.0;
        create_memo(move |_| (r.get().and_then(move |v| v.ok())))
    }

    pub fn watch_ok<F>(&self, immediate: bool, callback: F)
    where
        Q::Output: PartialEq,
        F: Fn(&Q::Output) + Clone + 'static,
    {
        let _ = watch(
            self.output_memo(),
            move |result, _, _| {
                if let Some(result) = result.as_ref() {
                    callback(result);
                }
            },
            immediate,
        );
    }
}

pub trait QueryState: Clone + Copy + Sized + 'static {
    type Env: Query;

    fn new(env: Shared<Self::Env>) -> Self;

    fn provide(env: Shared<Self::Env>) -> Self {
        let s = Self::new(env);
        provide_context(s);
        s
    }

    fn expect() -> Self {
        expect_context()
    }

    fn inner(&self) -> &UseQuery<Self::Env>;

    fn fetcher(&self) -> QueryFetcher<Self::Env> {
        self.configure_fetcher(self.inner().create_fetcher())
    }

    fn configure_fetcher(&self, b: QueryFetcher<Self::Env>) -> QueryFetcher<Self::Env> {
        b
    }
}

#[derive(Clone)]
pub struct UseQuery<Q: Query>(StoredValue<Shared<Q>>);

impl<Q: Query> Copy for UseQuery<Q> {}

impl<Q: Query> UseQuery<Q> {
    pub fn new(query: Shared<Q>) -> Self {
        Self(store_value(query))
    }
    pub fn create_fetcher(&self) -> QueryFetcher<Q> {
        QueryFetcher::new(self.0.get_value())
    }
}
