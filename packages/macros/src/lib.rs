use heck::ToUpperCamelCase;
use proc_macro::TokenStream;
use quote::{format_ident, quote, ToTokens};
use regex::Regex;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    Attribute, Data, DeriveInput, Fields, GenericArgument, Ident, ImplItem, ItemImpl, ItemStruct,
    Meta, Path, PathArguments, Token, Type, TypeParamBound,
};

/// Generate a Mutation trait & implementation for ['Mutation'].
/// The Input, Output and Err types must be in scope.
///
/// ```ignore
///
/// def_mutation_trait!(Hoge);
///
/// ```
#[proc_macro]
pub fn def_mutation_trait(input: TokenStream) -> TokenStream {
    let DefTraitInput { target_ident } = parse_macro_input!(input as DefTraitInput);
    let input_ident = format_ident!("{}Input", target_ident);
    let output_ident = format_ident!("{}Output", target_ident);
    let err_ident = format_ident!("{}Err", target_ident);

    let result = quote! {
        pub type #input_ident = Input;
        pub type #output_ident = Output;
        pub type #err_ident = Err;

        type Result = std::result::Result<Output, Err>;

        #[async_trait::async_trait(?Send)]
        pub trait #target_ident: DITarget {
            async fn exec(&self, input: Input) -> Result;
        }

        #[async_trait::async_trait(?Send)]
        impl Mutation for dyn #target_ident {
            type Input = Input;
            type Output = Output;
            type Err = Err;
            async fn exec(&self, input: Self::Input) -> Self::Result {
                (self as &dyn #target_ident).exec(input).await
            }
        }
    };

    result.into()
}

/// Generate a Query trait & implementation for ['Query'].
/// The Input, Output and Err types must be in scope.
///
/// ```ignore
///
/// def_query_trait!(Hoge);
///
/// ```
#[proc_macro]
pub fn def_query_trait(input: TokenStream) -> TokenStream {
    let DefTraitInput { target_ident } = parse_macro_input!(input as DefTraitInput);
    let input_ident = format_ident!("{}Input", target_ident);
    let output_ident = format_ident!("{}Output", target_ident);
    let err_ident = format_ident!("{}Err", target_ident);

    let result = quote! {
        pub type #input_ident = Input;
        pub type #output_ident = Output;
        pub type #err_ident = Err;
        type Result = std::result::Result<Output, Err>;

        #[cfg_attr(target_arch = "wasm32", async_trait::async_trait(?Send))]
        #[cfg_attr(not(target_arch = "wasm32"), async_trait::async_trait)]
        pub trait #target_ident: DITarget {
            async fn exec(&self, input: Input) -> Result;
        }

        #[cfg_attr(target_arch = "wasm32", async_trait::async_trait(?Send))]
        #[cfg_attr(not(target_arch = "wasm32"), async_trait::async_trait)]
        impl Query for dyn #target_ident {
            type Input = Input;
            type Output = Output;
            type Err = Err;
            async fn exec(&self, input: Self::Input) -> Self::Result {
                (self as &dyn #target_ident).exec(input).await
            }
        }
    };

    result.into()
}

/// Generate a LocalQuery trait & implementation for ['LocalQuery'].
/// The Input, Output and Err types must be in scope.
///
/// ```ignore
///
/// def_local_query_trait!(Hoge);
///
/// ```
#[proc_macro]
pub fn def_local_query_trait(input: TokenStream) -> TokenStream {
    let DefTraitInput { target_ident } = parse_macro_input!(input as DefTraitInput);
    let input_ident = format_ident!("{}Input", target_ident);
    let output_ident = format_ident!("{}Output", target_ident);
    let err_ident = format_ident!("{}Err", target_ident);

    let result = quote! {
        pub type #input_ident = Input;
        pub type #output_ident = Output;
        pub type #err_ident = Err;
        type Result = std::result::Result<Output, Err>;

        #[async_trait::async_trait(?Send)]
        pub trait #target_ident: DITarget {
            async fn exec(&self, input: Input) -> Result;
        }

        #[async_trait::async_trait(?Send)]
        impl LocalQuery for dyn #target_ident {
            type Input = Input;
            type Output = Output;
            type Err = Err;
            async fn exec(&self, input: Self::Input) -> Self::Result {
                (self as &dyn #target_ident).exec(input).await
            }
        }
    };

    result.into()
}

struct DefTraitInput {
    target_ident: syn::Ident,
}
impl Parse for DefTraitInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let target_ident = input.parse()?;
        Ok(DefTraitInput { target_ident })
    }
}

/// Generate a UseMutation's Env struct and implementation for ['UseMutation'].
///
/// Usage:
/// ```ignore
/// #[use_mutation]
/// pub struct UseHogeMutation {
///     pub foo: MutationAction<Foo>
///     pub bar: MutationAction<Bar>
/// }
/// ```
///
/// Generated codes are like this.
/// ```ignore
/// #[derive(DIPortal)]
/// pub struct UseHogeMutationEnv {
///     pub foo: DI<Foo>,
///     pub bar: DI<Bar>,
/// }
///
/// #[derive(derive_more::From, Clone)]
/// pub enum UseHogeMutationEvent {
///     Foo(FooOutput),
///     Bar(BarOutput),
/// }
///
/// impl UseMutation<UseHogeMutationEnv, UseHogeMutationEvent> for UseHogeMutation {
///     fn new(env: DI<UseHogeMutationEnv>, event_subscribers: &Vec<EventAction<#event_ident>>) -> Self {
///         let foo = MutationAction::create(&env.foo, event_subscribers);
///         let bar = MutationAction::create(&env.bar, event_subscribers);
///         Self {
///             foo,
///             bar,
///         }
///     }
/// }
/// ```
#[proc_macro_attribute]
pub fn use_mutation(_attr: TokenStream, tokens: TokenStream) -> TokenStream {
    let item: ItemStruct = parse_macro_input!(tokens);
    let ident = &item.ident;
    let env_ident = format_ident!("{}Env", &item.ident);
    let event_ident = format_ident!("{}Event", &item.ident);

    let env_q = {
        let env_fields = item
            .fields
            .iter()
            .map(|f| {
                let fname = &f.ident;
                let fty = ty_to_generic_args(&f.ty, "MutationAction")
                    .expect("field type must be MutationAction<T>");
                quote! {
                    pub #fname: portaldi::DI<#fty>
                }
            })
            .collect::<Vec<_>>();

        quote! {
            #[derive(portaldi::DIPortal)]
            pub struct #env_ident {
                #(#env_fields),*
            }
        }
    };
    let event_q = {
        let variants = item
            .fields
            .iter()
            .map(|f| {
                let fname = &f.ident.as_ref().unwrap();
                let variant_name = format_ident!("{}", fname.to_string().to_upper_camel_case());
                let binding = ty_to_generic_args(&f.ty, "MutationAction")
                    .expect("field type must be MutationAction<T>");
                let output_ident = format_ident!(
                    "{}Output",
                    binding
                        .first()
                        .unwrap()
                        .to_token_stream()
                        .to_string()
                        .replace("dyn ", "")
                );
                quote! {
                    #variant_name(#output_ident)
                }
            })
            .collect::<Vec<_>>();

        quote! {
            #[derive(derive_more::From, Clone)]
            pub enum #event_ident {
                #(#variants),*
            }
        }
    };

    let impl_use_mutation_q = {
        let fields = item.fields.iter().map(|f| {
            let fname = &f.ident;
            quote! {
                #fname: MutationAction::create(&env.#fname, event_subscribers)
            }
        });

        quote! {
            impl UseMutation<#env_ident, #event_ident> for #ident {

                fn create(
                    env: portaldi::DI<#env_ident>,
                    event_subscribers: &[EventAction<#event_ident>],
                ) -> Self {
                    Self {
                        #(#fields),*
                    }
                }
            }
        }
    };

    let result = quote! {
        #[derive(Clone, Copy)]
        #item

        #env_q
        #event_q

        #impl_use_mutation_q
    };

    println!("!!!! {}", &result);

    result.into()
}

/// Generate a implementation for ['UseQuery'].
///
/// Usage:
/// ```ignore
/// #[use_query]
/// pub struct UseHogeQuery(LocalQueryWrapper<HogeQuery>);
/// ```
///
/// Generated codes are like this.
/// ```ignore
/// impl UseQuery<HogeQuery> for UseHogeQuery {
///     fn new(query: DI<HogeQuery>) -> Self {
///         Self(QueryWrapper::new(query))
///     }
///     fn inner(&self) -> &QueryWrapper<HogeQuery> {
///         &self.0
///     }
/// }
/// ```
#[proc_macro_attribute]
pub fn use_query(_attr: TokenStream, tokens: TokenStream) -> TokenStream {
    let item: ItemStruct = parse_macro_input!(tokens);
    let ident = &item.ident;

    let impl_use_query_q = {
        let msg = "Fields muat be (LocalQueryWrapper<Query>";
        let query_ty = match &item.fields {
            Fields::Unnamed(fs) => {
                ty_to_generic_args(&fs.unnamed.first().expect(msg).ty, "QueryWrapper").expect(msg)
            }
            _ => panic!("{}", msg), // TODO
        };

        quote! {
            impl UseQuery<#query_ty> for #ident {
                fn new(query: portaldi::DI<#query_ty>) -> Self {
                    Self(QueryWrapper::new(query))
                }
                fn inner(&self) -> &QueryWrapper<#query_ty> {
                    &self.0
                }
            }
        }
    };

    let result = quote! {
        #[derive(Clone, Copy)]
        #item
        #impl_use_query_q
    };

    result.into()
}

/// Generate a implementation for ['UseLocalQuery'].
///
/// Usage:
/// ```ignore
/// #[use_local_query]
/// pub struct UseHogeQuery(LocalQueryWrapper<HogeQuery>, RwSignal<HogeState>);
/// ```
///
/// Generated codes are like this.
/// ```ignore
/// impl UseLocalQuery<HogeQuery> for UseHogeQuery {
///     fn new(query: DI<HogeQuery>) -> Self {
///         Self(QueryWrapper::new(query), create_rw_signal(HogeState::default()))
///     }
///     fn inner(&self) -> &QueryWrapper<HogeQuery> {
///         &self.0
///     }
///     fn state(&self) -> RwSignal<HogeState> {
///         self.1
///     }
/// }
/// ```
#[proc_macro_attribute]
pub fn use_local_query(_attr: TokenStream, tokens: TokenStream) -> TokenStream {
    let item: ItemStruct = parse_macro_input!(tokens);
    let ident = &item.ident;

    let impl_use_query_q = {
        let msg = "Fields muat be (LocalQueryWrapper<Query>, RwSignal<State>)";
        let (query_ty, state_ty) = match &item.fields {
            Fields::Unnamed(fs) => {
                let query_ty =
                    ty_to_generic_args(&fs.unnamed.first().expect(msg).ty, "LocalQueryWrapper")
                        .expect(msg);
                let state_ty =
                    ty_to_generic_args(&fs.unnamed.last().expect(msg).ty, "RwSignal").expect(msg);
                (query_ty, state_ty)
            }
            _ => panic!("{}", msg), // TODO
        };

        quote! {
            impl UseLocalQuery<#query_ty, #state_ty> for #ident {
                fn new(query: portaldi::DI<#query_ty>) -> Self {
                    Self(LocalQueryWrapper::new(query), create_rw_signal(#state_ty::default()))
                }
                fn inner(&self) -> &LocalQueryWrapper<#query_ty> {
                    &self.0
                }
                fn state(&self) -> Signal<#state_ty> {
                    self.1.into()
                }
            }
        }
    };

    let result = quote! {
        #[derive(Clone, Copy)]
        #item
        #impl_use_query_q
    };

    // println!("{:?}", result.to_string());

    result.into()
}

fn ty_to_generic_args<'a>(
    ty: &Type,
    container_type_name: &str,
) -> Option<Punctuated<GenericArgument, Token![,]>> {
    match ty {
        Type::Path(path) => {
            let p = path.path.segments.first();
            if p.map(|p| p.ident.to_string()) == Some(container_type_name.to_string()) {
                match &p.unwrap().arguments {
                    PathArguments::AngleBracketed(args) => Some(args.args.clone()),
                    _ => None,
                }
            } else {
                None
            }
        }
        _ => None,
    }
}

#[proc_macro_attribute]
pub fn async_trait_for_query(_attr: TokenStream, tokens: TokenStream) -> TokenStream {
    let item_impl = parse_macro_input!(tokens as ItemImpl);

    let result = quote! {
        #[cfg_attr(target_arch = "wasm32", async_trait::async_trait(?Send))]
        #[cfg_attr(not(target_arch = "wasm32"), async_trait::async_trait)]
        #item_impl
    };

    result.into()
}
