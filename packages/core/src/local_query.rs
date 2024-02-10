use async_trait::async_trait;
use leptos::*;

use crate::utils::*;

#[async_trait(?Send)]
pub trait LocalQuery: 'static {
    type Input: Clone + PartialEq + 'static;
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
    pub fn with_watch_ok(&mut self, callback: impl Fn(Q::Output) + 'static) -> &mut Self {
        self.on_ok = store_value(Some(Shared::new(callback)));
        self
    }
    pub fn with_watch_err(&mut self, callback: impl Fn(Q::Err) + 'static) -> &mut Self {
        self.on_err = store_value(Some(Shared::new(callback)));
        self
    }

    pub fn run(&self, input: Q::Input) -> LocalQueryResource<Q> {
        let query = self.query;
        let on_ok = self.on_ok;
        let on_err = self.on_err;
        let res = LocalQueryResource(create_local_resource(move || input.clone(), {
            move |input| Self::fetch(query.get_value().0, on_ok, on_err, input)
        }));
        self.query.get_value().1.set(Some(res.clone()));
        res
    }
    pub fn run_with_memo(&self, input: Memo<Q::Input>) -> LocalQueryResource<Q> {
        let query = self.query;
        let on_ok = self.on_ok;
        let on_err = self.on_err;
        let res = LocalQueryResource(create_local_resource(input, {
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

pub trait UseLocalQuery<Q: LocalQuery + ?Sized, S>:
    InitializeLocalQueryFetcher<Q> + Clone + Copy + Sized + 'static
{
    fn new(env: Shared<Q>) -> Self;
    fn inner(&self) -> &LocalQueryWrapper<Q>;
    fn state(&self) -> Signal<S>;

    fn provide(env: Shared<Q>) -> Self {
        let s = Self::new(env);
        provide_context(s);
        s
    }

    fn expect() -> Self {
        expect_context()
    }

    fn expect_with<T>(f: impl Fn(LocalQueryFetcher<Q>) -> T) -> (Self, Signal<S>, T) {
        let _self = Self::expect();
        let res = f(_self.fetcher());
        (_self, _self.state(), res)
    }

    fn fetcher(&self) -> LocalQueryFetcher<Q> {
        let mut fetcher = self.inner().create_fetcher();
        self.initialize_fetcher(&mut fetcher);
        fetcher
    }

    fn last_output(&self) -> Option<Q::Output> {
        let x = self.inner().1;
        x.get().and_then(|r| r.value())
    }
}

pub trait InitializeLocally<T: Copy + 'static> {
    fn initialize(&self, params: T) -> Effect<()>
    where
        Self: Clone + 'static,
    {
        let _self = self.clone();
        create_render_effect(move |_| _self.do_initialize(params))
    }

    fn do_initialize(&self, params: T);
}

// #[derive(Clone)]
pub struct LocalQueryWrapper<Q: LocalQuery + ?Sized>(
    StoredValue<Shared<Q>>,
    RwSignal<Option<LocalQueryResource<Q>>>,
);

impl<Q: LocalQuery + ?Sized> Clone for LocalQueryWrapper<Q> {
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone())
    }
}
impl<Q: LocalQuery + ?Sized> Copy for LocalQueryWrapper<Q> {}

impl<Q: LocalQuery + ?Sized> LocalQueryWrapper<Q> {
    pub fn new(query: Shared<Q>) -> Self {
        Self(store_value(query), create_rw_signal(None))
    }
    pub fn create_fetcher(&self) -> LocalQueryFetcher<Q> {
        LocalQueryFetcher::new(self.clone())
    }
}
