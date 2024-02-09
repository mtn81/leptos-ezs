use proc_macro::TokenStream;
use quote::{format_ident, quote};
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

    let result = quote! {
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

    let result = quote! {
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
/// impl UseMutation<UseHogeMutationEnv> for UseHogeMutation {
///     fn new(env: DI<UseHogeMutationEnv>) -> Self {
///         let foo = MutationAction::new(&env.foo);
///         let bar = MutationAction::new(&env.bar);
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

    let impl_use_mutation_q = {
        let fields = item.fields.iter().map(|f| {
            let fname = &f.ident;
            quote! {
                #fname: MutationAction::new(&env.#fname)
            }
        });

        quote! {
            impl UseMutation<#env_ident> for #ident {
                fn new(env: portaldi::DI<#env_ident>) -> Self {
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
        #impl_use_mutation_q
    };

    // println!("!!!! {}", &result);

    result.into()
}

/// Generate a implementation for ['UseQuery'].
///
/// Usage:
/// ```ignore
/// #[use_query]
/// pub struct UseHogeQuery(QueryWrapper<HogeQuery>, RwSignal<HogeState>);
/// ```
///
/// Generated codes are like this.
/// ```ignore
/// impl UseQuery<HogeQuery> for UseHogeQuery {
///     fn new(query: DI<HogeQuery>) -> Self {
///         Self(QueryWrapper::new(query), create_rw_signal(HogeState::default()))
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
        let msg = "Fields muat be (QueryWrapper<Query>, RwSignal<State>)";
        let (query_ty, state_ty) = match &item.fields {
            Fields::Unnamed(fs) => {
                let query_ty =
                    ty_to_generic_args(&fs.unnamed.first().expect(msg).ty, "QueryWrapper")
                        .expect(msg);
                let state_ty =
                    ty_to_generic_args(&fs.unnamed.last().expect(msg).ty, "RwSignal").expect(msg);
                (query_ty, state_ty)
            }
            _ => panic!("{}", msg), // TODO
        };

        quote! {
            impl UseQuery<#query_ty> for #ident {
                fn new(query: portaldi::DI<#query_ty>) -> Self {
                    Self(QueryWrapper::new(query), create_rw_signal(#state_ty::default()))
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
pub fn with_async_trait(_attr: TokenStream, tokens: TokenStream) -> TokenStream {
    let item_impl = parse_macro_input!(tokens as ItemImpl);

    let result = quote! {
        #[cfg_attr(target_arch = "wasm32", async_trait::async_trait(?Send))]
        #[cfg_attr(not(target_arch = "wasm32"), async_trait::async_trait)]
        #item_impl
    };

    result.into()
}
