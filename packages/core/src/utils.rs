use leptos::*;

#[cfg(target_arch = "wasm32")]
pub(crate) type Shared<T> = std::rc::Rc<T>;
#[cfg(not(target_arch = "wasm32"))]
pub(crate) type Shared<T> = std::sync::Arc<T>;

pub fn option_to_signal<T>(v: Option<T>) -> Option<RwSignal<T>> {
    v.map(|w| create_rw_signal(w))
}

#[derive(Clone, Debug)]
pub struct IdentifiedRwSignal<ID: PartialEq + Clone + 'static, D: Clone + 'static> {
    pub id: ID,
    pub sig: RwSignal<D>,
}

impl<ID: PartialEq + Clone, D: Clone> IdentifiedRwSignal<ID, D> {
    pub fn create(id: &ID, data: &D) -> Self {
        Self {
            id: id.clone(),
            sig: create_rw_signal(data.clone()),
        }
    }
}

#[derive(Clone, Debug)]
pub struct IdentifiedSignal<ID: PartialEq + Clone + 'static, D: Clone + 'static> {
    pub id: ID,
    pub sig: Signal<D>,
}

impl<ID: PartialEq + Clone, D: Clone> From<&IdentifiedRwSignal<ID, D>> for IdentifiedSignal<ID, D> {
    fn from(value: &IdentifiedRwSignal<ID, D>) -> Self {
        Self {
            id: value.id.clone(),
            sig: value.sig.into_signal(),
        }
    }
}

pub type VecRwSignals<ID, D> = Vec<IdentifiedRwSignal<ID, D>>;
pub type VecSignals<ID, D> = Vec<IdentifiedSignal<ID, D>>;

pub mod vec_ops {
    use super::*;

    pub fn create_rw_signals<ID: PartialEq + Clone, D: Clone>(
        vec: &Vec<D>,
        id_fn: impl Fn(&D) -> &ID,
    ) -> VecRwSignals<ID, D> {
        vec.iter()
            .map(|data| IdentifiedRwSignal::create(id_fn(data), data))
            .collect()
    }

    pub fn to_signals<ID: PartialEq + Clone, D: Clone>(
        rw_sigs: &VecRwSignals<ID, D>,
    ) -> VecSignals<ID, D> {
        rw_sigs.iter().map(IdentifiedSignal::from).collect()
    }

    pub fn append<ID: PartialEq + Clone, D: Clone>(
        state: RwSignal<VecRwSignals<ID, D>>,
        id: &ID,
        data: &D,
    ) {
        state.update(|vec| vec.push(IdentifiedRwSignal::create(id, data)));
    }
    pub fn update<ID: PartialEq + Clone, D: Clone>(
        state: RwSignal<VecRwSignals<ID, D>>,
        id: &ID,
        data: &D,
    ) {
        if let Some(data_) = state.get_untracked().into_iter().find(|sig| &sig.id == id) {
            data_.sig.set(data.clone());
        }
    }
    pub fn remove<ID: PartialEq + Clone, D: Clone>(state: RwSignal<VecRwSignals<ID, D>>, id: &ID) {
        state.update(|vec| vec.retain(|sig| &sig.id != id))
    }
}

