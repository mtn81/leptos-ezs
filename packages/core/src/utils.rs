use leptos::*;

#[cfg(target_arch = "wasm32")]
pub(crate) type Shared<T> = std::rc::Rc<T>;
#[cfg(not(target_arch = "wasm32"))]
pub(crate) type Shared<T> = std::sync::Arc<T>;

pub fn vec_to_signal<T>(v: Vec<T>) -> Vec<RwSignal<T>> {
    v.into_iter().map(|w| create_rw_signal(w)).collect()
}
pub fn option_to_signal<T>(v: Option<T>) -> Option<RwSignal<T>> {
    v.map(|w| create_rw_signal(w))
}
