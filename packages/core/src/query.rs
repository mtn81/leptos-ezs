use std::collections::HashMap;

use async_trait::async_trait;
use leptos::*;
use serde::de::DeserializeOwned;

use crate::utils::*;

#[cfg_attr(target_arch = "wasm32", async_trait(?Send))]
#[cfg_attr(not(target_arch = "wasm32"), async_trait)]
pub trait Query: 'static {
    type Input: Clone + Eq + std::hash::Hash + 'static;
    type Output: Clone + serde::Serialize + DeserializeOwned + 'static;
    type Err: Clone + serde::Serialize + DeserializeOwned + 'static;
    type Result = Result<Self::Output, Self::Err>;

    async fn exec(&self, input: Self::Input) -> Result<Self::Output, Self::Err>;
}

// #[derive(Clone)]
pub struct QueryFetcher<Q: Query + ?Sized> {
    query: StoredValue<QueryWrapper<Q>>,
}

impl<Q: Query + ?Sized> Clone for QueryFetcher<Q> {
    fn clone(&self) -> Self {
        Self {
            query: self.query.clone(),
        }
    }
}
impl<Q: Query + ?Sized> Copy for QueryFetcher<Q> {}

impl<Q: Query + ?Sized> QueryFetcher<Q> {
    pub fn new(query: QueryWrapper<Q>) -> Self {
        Self {
            query: store_value(query),
        }
    }

    pub fn run(&self, input: impl Fn() -> Q::Input + Clone + 'static) -> QueryResource<Q> {
        let query = self.query;
        let res = QueryResource(create_resource(input.clone(), {
            move |input| Self::fetch(query.get_value().0, input)
        }));
        self.query.get_value().1.update(move |map| {
            map.insert(input(), res.clone());
        });
        // self.query.get_value().1.set(Some(res.clone()));
        res
    }
    pub fn run_with_memo(&self, input: Memo<Q::Input>) -> QueryResource<Q> {
        let query = self.query;
        let res = QueryResource(create_resource(input, {
            move |input| Self::fetch(query.get_value().0, input)
        }));
        self.query.get_value().1.update(move |map| {
            map.insert(input(), res.clone());
        });
        // self.query.get_value().1.set(Some(res.clone()));
        res
    }

    async fn fetch(query: StoredValue<Shared<Q>>, input: Q::Input) -> Result<Q::Output, Q::Err> {
        logging::log!("Fetching query");
        // TODO debounce, cache, retry handling
        let result = query.get_value().exec(input).await;
        result
    }
}

// #[derive(Clone, Copy)]
pub struct QueryResource<Q: Query + ?Sized>(Resource<Q::Input, Result<Q::Output, Q::Err>>);

impl<Q: Query + ?Sized> Clone for QueryResource<Q> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
impl<Q: Query + ?Sized> Copy for QueryResource<Q> {}

impl<Q: Query + ?Sized> QueryResource<Q> {
    pub fn value(&self) -> Option<Q::Output> {
        let r = self.0;
        r.get().and_then(move |v| v.ok())
    }

    pub fn memo(&self) -> Memo<Option<Q::Output>>
    where
        Q::Output: PartialEq,
    {
        let r = self.0;
        create_memo(move |_| (r.get().and_then(move |v| v.ok())))
    }
}

pub trait UseQuery<Q: Query + ?Sized>: Clone + Copy + Sized + 'static {
    fn new(env: Shared<Q>) -> Self;

    fn provide(env: Shared<Q>) -> Self {
        let s = Self::new(env);
        provide_context(s);
        s
    }

    fn expect() -> Self {
        expect_context()
    }

    fn expect_with<T>(f: impl Fn(QueryFetcher<Q>) -> T) -> (T, Self) {
        let _self = Self::expect();
        let res = f(_self.create_fetcher());
        (res, _self)
    }

    fn inner(&self) -> &QueryWrapper<Q>;

    fn create_fetcher(&self) -> QueryFetcher<Q> {
        self.inner().create_fetcher()
    }

    fn last_output(&self, input: Q::Input) -> Option<Q::Output> {
        let x = self.inner().1;
        x.with(move |map| map.get(&input).and_then(|r| r.value()))
    }
}

// #[derive(Clone)]
pub struct QueryWrapper<Q: Query + ?Sized>(
    StoredValue<Shared<Q>>,
    RwSignal<HashMap<Q::Input, QueryResource<Q>>>,
);

impl<Q: Query + ?Sized> Clone for QueryWrapper<Q> {
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone())
    }
}
impl<Q: Query + ?Sized> Copy for QueryWrapper<Q> {}

impl<Q: Query + ?Sized> QueryWrapper<Q> {
    pub fn new(query: Shared<Q>) -> Self {
        Self(store_value(query), create_rw_signal(HashMap::default()))
    }
    pub fn create_fetcher(&self) -> QueryFetcher<Q> {
        QueryFetcher::new(self.clone())
    }
}
