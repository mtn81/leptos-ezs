use std::collections::HashMap;

use async_trait::async_trait;
use leptos::*;

use crate::{utils::*, EventAction};

#[async_trait(?Send)]
pub trait LocalQuery: 'static {
    type Input: Clone + Eq + std::hash::Hash + 'static;
    type Output: Clone + 'static;
    type Err: Clone + 'static;
    type Result = Result<Self::Output, Self::Err>;

    async fn exec(&self, input: Self::Input) -> Result<Self::Output, Self::Err>;
}

// #[derive(Clone)]
pub struct LocalQueryFetcher<Q: LocalQuery + ?Sized> {
    query: StoredValue<LocalQueryWrapper<Q>>,
    on_ok: StoredValue<Option<Shared<dyn Fn(Q::Output)>>>,
    on_err: StoredValue<Option<Shared<dyn Fn(Q::Err)>>>,
}

impl<Q: LocalQuery + ?Sized> Clone for LocalQueryFetcher<Q> {
    fn clone(&self) -> Self {
        Self {
            query: self.query.clone(),
            on_ok: self.on_ok.clone(),
            on_err: self.on_err.clone(),
        }
    }
}
impl<Q: LocalQuery + ?Sized> Copy for LocalQueryFetcher<Q> {}

impl<Q: LocalQuery + ?Sized> LocalQueryFetcher<Q> {
    pub fn new(query: LocalQueryWrapper<Q>) -> Self {
        Self {
            query: store_value(query),
            on_ok: store_value(None),
            on_err: store_value(None),
        }
    }
    pub fn on_ok(&mut self, callback: impl Fn(Q::Output) + 'static) -> &mut Self {
        self.on_ok = store_value(Some(Shared::new(callback)));
        self
    }
    pub fn on_err(&mut self, callback: impl Fn(Q::Err) + 'static) -> &mut Self {
        self.on_err = store_value(Some(Shared::new(callback)));
        self
    }

    pub fn run(&self, input: impl Fn() -> Q::Input + Clone + 'static) -> LocalQueryResource<Q> {
        let query = self.query;
        let on_ok = self.on_ok;
        let on_err = self.on_err;
        let res = LocalQueryResource(create_local_resource(input.clone(), {
            move |input| Self::fetch(query.get_value().0, on_ok, on_err, input)
        }));
        self.query.get_value().1.update(move |map| {
            map.insert(input(), res.clone());
        });
        // self.query.get_value().1.set(Some(res.clone()));
        res
    }
    pub fn run_with_memo(&self, input: Memo<Q::Input>) -> LocalQueryResource<Q> {
        let query = self.query;
        let on_ok = self.on_ok;
        let on_err = self.on_err;
        let res = LocalQueryResource(create_local_resource(input, {
            move |input| Self::fetch(query.get_value().0, on_ok, on_err, input)
        }));
        self.query.get_value().1.update(move |map| {
            map.insert(input(), res.clone());
        });
        // self.query.get_value().1.set(Some(res.clone()));
        res
    }

    async fn fetch(
        query: StoredValue<Shared<Q>>,
        on_ok: StoredValue<Option<Shared<dyn Fn(Q::Output)>>>,
        on_err: StoredValue<Option<Shared<dyn Fn(Q::Err)>>>,
        input: Q::Input,
    ) -> Result<Q::Output, Q::Err> {
        logging::log!("Fetching local query");
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
pub struct LocalQueryResource<Q: LocalQuery + ?Sized>(
    Resource<Q::Input, Result<Q::Output, Q::Err>>,
);

impl<Q: LocalQuery + ?Sized> Clone for LocalQueryResource<Q> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
impl<Q: LocalQuery + ?Sized> Copy for LocalQueryResource<Q> {}

impl<Q: LocalQuery + ?Sized> LocalQueryResource<Q> {
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

    pub fn watch_ok<F>(&self, immediate: bool, callback: F)
    where
        Q::Output: PartialEq,
        F: Fn(&Q::Output) + Clone + 'static,
    {
        let _ = watch(
            self.memo(),
            move |result, _, _| {
                if let Some(result) = result.as_ref() {
                    callback(result);
                }
            },
            immediate,
        );
    }
}

pub trait InitializeLocalQueryFetcher<Q: LocalQuery + ?Sized> {
    fn initialize_fetcher(&self, _fetcher: &mut LocalQueryFetcher<Q>) {}
}

pub trait UseLocalQuery<Q: LocalQuery + ?Sized>:
    InitializeLocalQueryFetcher<Q> + Clone + Copy + Sized + 'static
{
    fn new(env: Shared<Q>) -> Self;
    fn inner(&self) -> &LocalQueryWrapper<Q>;

    fn provide(env: Shared<Q>) -> Self {
        let s = Self::new(env);
        provide_context(s);
        s
    }

    fn expect() -> Self {
        expect_context()
    }

    fn expect_with<T>(f: impl Fn(LocalQueryFetcher<Q>) -> T) -> (T, Self) {
        let _self = Self::expect();
        let res = f(_self.fetcher());
        (res, _self)
    }

    fn fetcher(&self) -> LocalQueryFetcher<Q> {
        let mut fetcher = self.inner().create_fetcher();
        self.initialize_fetcher(&mut fetcher);
        fetcher
    }

    fn last_output(&self, input: Q::Input) -> Option<Q::Output> {
        let x = self.inner().1;
        x.with(move |map| map.get(&input).and_then(|r| r.value()))
    }
}

pub trait LocalQueryStateView<Q: LocalQuery + ?Sized, V>: UseLocalQuery<Q> {
    fn state_view(&self) -> Signal<V>;

    fn expect_state() -> (Signal<V>, Self) {
        let _self = Self::expect();
        (_self.state_view(), _self)
    }
    fn expect_state_with<T>(f: impl Fn(LocalQueryFetcher<Q>) -> T) -> (Signal<V>, T, Self) {
        let _self = Self::expect();
        let res = f(_self.fetcher());
        (_self.state_view(), res, _self)
    }
}

pub trait EventSubsciber<E: Clone + 'static> {
    fn create_event_subscriber(&self) -> EventAction<E>
    where
        Self: Copy + 'static,
    {
        let _self = self.clone();
        create_action(move |e: &E| {
            let e = e.clone();
            async move { _self.on_event(e) }
        })
    }

    fn on_event(&self, event: E);
}

// #[derive(Clone)]
pub struct LocalQueryWrapper<Q: LocalQuery + ?Sized>(
    StoredValue<Shared<Q>>,
    RwSignal<HashMap<Q::Input, LocalQueryResource<Q>>>,
);

impl<Q: LocalQuery + ?Sized> Clone for LocalQueryWrapper<Q> {
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone())
    }
}
impl<Q: LocalQuery + ?Sized> Copy for LocalQueryWrapper<Q> {}

impl<Q: LocalQuery + ?Sized> LocalQueryWrapper<Q> {
    pub fn new(query: Shared<Q>) -> Self {
        Self(store_value(query), create_rw_signal(HashMap::default()))
    }
    pub fn create_fetcher(&self) -> LocalQueryFetcher<Q> {
        LocalQueryFetcher::new(self.clone())
    }
}
