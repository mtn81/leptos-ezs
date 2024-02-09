use async_trait::async_trait;
use leptos::*;
use serde::de::DeserializeOwned;

use crate::utils::*;

#[cfg_attr(target_arch = "wasm32", async_trait(?Send))]
#[cfg_attr(not(target_arch = "wasm32"), async_trait)]
pub trait Query: 'static {
    type Input: Clone + PartialEq + 'static;
    type Output: Clone + serde::Serialize + DeserializeOwned + 'static;
    type Err: Clone + serde::Serialize + DeserializeOwned + 'static;
    type Result = Result<Self::Output, Self::Err>;

    async fn exec(&self, input: Self::Input) -> Result<Self::Output, Self::Err>;
}

// #[derive(Clone)]
pub struct QueryFetcher<Q: Query + ?Sized> {
    query: StoredValue<QueryWrapper<Q>>,
    on_ok: StoredValue<Option<Shared<dyn Fn(Q::Output)>>>,
    on_err: StoredValue<Option<Shared<dyn Fn(Q::Err)>>>,
}

impl<Q: Query + ?Sized> Clone for QueryFetcher<Q> {
    fn clone(&self) -> Self {
        Self {
            query: self.query.clone(),
            on_ok: self.on_ok.clone(),
            on_err: self.on_err.clone(),
        }
    }
}
impl<Q: Query + ?Sized> Copy for QueryFetcher<Q> {}

impl<Q: Query + ?Sized> QueryFetcher<Q> {
    pub fn new(query: QueryWrapper<Q>) -> Self {
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
        let res = ResourceW(create_resource(move || input.clone(), {
            move |input| Self::fetch(query.get_value().0, on_ok, on_err, input)
        }));
        self.query.get_value().1.set(Some(res.clone()));
        res
    }
    pub fn run_with_memo(&self, input: Memo<Q::Input>) -> ResourceW<Q> {
        let query = self.query;
        let on_ok = self.on_ok;
        let on_err = self.on_err;
        let res = ResourceW(create_resource(input, {
            move |input| Self::fetch(query.get_value().0, on_ok, on_err, input)
        }));
        self.query.get_value().1.set(Some(res.clone()));
        res
    }

    async fn fetch(
        query: StoredValue<Shared<Q>>,
        on_ok: StoredValue<Option<Shared<dyn Fn(Q::Output)>>>,
        on_err: StoredValue<Option<Shared<dyn Fn(Q::Err)>>>,
        input: Q::Input,
    ) -> Result<Q::Output, Q::Err> {
        logging::log!("!!! fetch !!!");
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

// #[derive(Clone, Copy)]
pub struct ResourceW<Q: Query + ?Sized>(Resource<Q::Input, Result<Q::Output, Q::Err>>);

impl<Q: Query + ?Sized> Clone for ResourceW<Q> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
impl<Q: Query + ?Sized> Copy for ResourceW<Q> {}

impl<Q: Query + ?Sized> ResourceW<Q> {
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

pub trait QueryFetcherModifier<Q: Query + ?Sized> {
    fn modify_fetcher(&self, fetcher: QueryFetcher<Q>) -> QueryFetcher<Q> {
        fetcher
    }
}

pub trait UseQuery<Q: Query + ?Sized>:
    QueryFetcherModifier<Q> + Clone + Copy + Sized + 'static
{
    fn new(env: Shared<Q>) -> Self;

    fn provide(env: Shared<Q>) -> Self {
        let s = Self::new(env);
        provide_context(s);
        s
    }

    fn expect() -> Self {
        expect_context()
    }

    fn inner(&self) -> &QueryWrapper<Q>;

    fn fetcher(&self) -> QueryFetcher<Q> {
        self.modify_fetcher(self.inner().create_fetcher())
    }

    fn last_output(&self) -> Signal<Option<Q::Output>> {
        let x = self.inner().1;
        Signal::derive(move || x.get().and_then(|r| r.output().get()))
    }
}

// #[derive(Clone)]
pub struct QueryWrapper<Q: Query + ?Sized>(StoredValue<Shared<Q>>, RwSignal<Option<ResourceW<Q>>>);

impl<Q: Query + ?Sized> Clone for QueryWrapper<Q> {
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone())
    }
}
impl<Q: Query + ?Sized> Copy for QueryWrapper<Q> {}

impl<Q: Query + ?Sized> QueryWrapper<Q> {
    pub fn new(query: Shared<Q>) -> Self {
        Self(store_value(query), create_rw_signal(None))
    }
    pub fn create_fetcher(&self) -> QueryFetcher<Q> {
        QueryFetcher::new(self.clone())
    }
}
