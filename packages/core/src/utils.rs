use std::collections::HashMap;

use leptos::*;

#[cfg(target_arch = "wasm32")]
pub type Shared<T> = std::rc::Rc<T>;
#[cfg(not(target_arch = "wasm32"))]
pub type Shared<T> = std::sync::Arc<T>;

pub fn option_to_signal<T>(v: Option<T>) -> Option<RwSignal<T>> {
    v.map(|w| create_rw_signal(w))
}

#[derive(Clone, Debug)]
pub struct IdentifiedRwSignal<ID: PartialEq + Clone + 'static, D: Clone + 'static> {
    pub id: ID,
    pub sig: RwSignal<D>,
}

impl<ID: PartialEq + Clone, D: Clone> IdentifiedRwSignal<ID, D> {
    pub fn create_(id: &ID, data: RwSignal<D>) -> Self {
        Self {
            id: id.clone(),
            sig: data,
        }
    }
    pub fn create(id: &ID, data: &D) -> Self {
        let sig = create_rw_signal(data.clone());
        Self {
            id: id.clone(),
            sig,
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
            sig: value.sig.into(),
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

    pub fn to_read_only<ID: PartialEq + Clone, D: Clone>(
        s: RwSignal<VecRwSignals<ID, D>>,
    ) -> Signal<VecSignals<ID, D>> {
        Signal::derive(move || s.with(to_signals))
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
        state.update(|vec| append_(vec, id, data))
    }
    pub fn append_<ID: PartialEq + Clone, D: Clone>(
        vec: &mut VecRwSignals<ID, D>,
        id: &ID,
        data: &D,
    ) {
        vec.push(IdentifiedRwSignal::create(id, data));
    }

    pub fn update<ID: PartialEq + Clone, D: Clone>(
        state: RwSignal<VecRwSignals<ID, D>>,
        id: &ID,
        data: &D,
    ) -> bool {
        let b = state.try_update(|vec| update_(vec, id, data));
        b.unwrap_or(false)
    }
    pub fn update_<ID: PartialEq + Clone, D: Clone>(
        vec: &mut VecRwSignals<ID, D>,
        id: &ID,
        data: &D,
    ) -> bool {
        if let Some(data_) = vec.into_iter().find(|sig| &sig.id == id) {
            data_.sig.set(data.clone());
            true
        } else {
            false
        }
    }

    pub fn save<ID: PartialEq + Clone, D: Clone>(
        state: RwSignal<VecRwSignals<ID, D>>,
        id: &ID,
        data: &D,
    ) {
        state.update(|vec| save_(vec, id, data))
    }
    pub fn save_<ID: PartialEq + Clone, D: Clone>(
        vec: &mut VecRwSignals<ID, D>,
        id: &ID,
        data: &D,
    ) {
        if !update_(vec, id, data) {
            append_(vec, id, data)
        }
    }

    pub fn remove<ID: PartialEq + Clone, D: Clone>(state: RwSignal<VecRwSignals<ID, D>>, id: &ID) {
        state.update(|vec| remove_(vec, id))
    }
    pub fn remove_<ID: PartialEq + Clone, D: Clone>(vec: &mut VecRwSignals<ID, D>, id: &ID) {
        vec.retain(|sig| &sig.id != id);
    }
}

pub type HashMapRwSignals<ID, D> = HashMap<ID, RwSignal<D>>;
pub type HashMapSignals<ID, D> = HashMap<ID, Signal<D>>;

pub mod hashmap_ops {
    use super::*;

    pub fn to_read_only<ID: Eq + std::hash::Hash + Clone, D: Clone>(
        s: RwSignal<HashMapRwSignals<ID, D>>,
    ) -> Signal<HashMapSignals<ID, D>> {
        Signal::derive(move || s.with(to_signals))
    }

    pub fn to_signals<ID: Eq + std::hash::Hash + Clone, D: Clone>(
        rw_sigs: &HashMapRwSignals<ID, D>,
    ) -> HashMapSignals<ID, D> {
        rw_sigs
            .iter()
            .map(|(k, v)| (k.clone(), v.clone().into_signal()))
            .collect()
    }

    pub fn insert<ID: Eq + std::hash::Hash + Clone, D: Clone>(
        state: RwSignal<HashMapRwSignals<ID, D>>,
        id: &ID,
        data: &D,
    ) {
        state.update(|map| {
            map.insert(id.clone(), create_rw_signal(data.clone()));
        })
    }

    pub fn insert_option<ID: Eq + std::hash::Hash + Clone, D: Clone>(
        state: RwSignal<HashMapRwSignals<ID, D>>,
        kv: &Option<(ID, D)>,
    ) {
        state.update(|map| {
            if let Some((id, data)) = kv {
                map.insert(id.clone(), create_rw_signal(data.clone()));
            }
        })
    }

    pub fn signal_option_by_id<ID: Eq + std::hash::Hash + Clone, D: Clone, T>(
        sigs: Signal<HashMapSignals<ID, D>>,
        id: ID,
        f: impl Fn(&D) -> T + 'static,
    ) -> Signal<Option<T>> {
        Signal::derive(move || sigs.with(|map| map.get(&id).map(|s| f(&s.get()))))
    }
    pub fn signal_by_id<ID: Eq + std::hash::Hash + Clone, D: Clone, T>(
        sigs: Signal<HashMapSignals<ID, D>>,
        id: ID,
        f: impl Fn(&D) -> T + 'static,
        default_f: impl FnOnce() -> T + Copy + 'static,
    ) -> Signal<T> {
        Signal::derive(move || {
            sigs.with(|map| map.get(&id).map(|s| f(&s.get())))
                .unwrap_or_else(default_f)
        })
    }
}
