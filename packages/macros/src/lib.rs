use proc_macro::TokenStream;
use quote::{format_ident, quote};
use regex::Regex;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    Attribute, Data, DeriveInput, GenericArgument, Ident, ImplItem, ItemImpl, ItemStruct, Meta,
    Path, PathArguments, Token, Type, TypeParamBound,
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

        #[async_trait(?Send)]
        pub trait #target_ident: DITarget {
            async fn exec(&self, input: Input) -> Result;
        }

        #[async_trait(?Send)]
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

        #[cfg_attr(target_arch = "wasm32", async_trait(?Send))]
        #[cfg_attr(not(target_arch = "wasm32"), async_trait)]
        pub trait #target_ident: DITarget {
            async fn exec(&self, input: Input) -> Result;
        }

        #[cfg_attr(target_arch = "wasm32", async_trait(?Send))]
        #[cfg_attr(not(target_arch = "wasm32"), async_trait)]
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
                let fty = match &f.ty {
                    Type::Path(path) => {
                        let p = path.path.segments.first();
                        if p.map(|p| p.ident.to_string()) == Some("MutationAction".to_string()) {
                            match &p.unwrap().arguments {
                                PathArguments::AngleBracketed(args) => &args.args,
                                _ => panic!("field type must be MutationAction<T>"),
                            }
                        } else {
                            panic!("field type must be MutationAction<T>")
                        }
                    }
                    _ => panic!("field type must be MutationAction<T>"), // TODO
                };
                quote! {
                    pub #fname: DI<#fty>
                }
            })
            .collect::<Vec<_>>();

        quote! {
            #[derive(DIPortal)]
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
                fn new(env: DI<#env_ident>) -> Self {
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

    println!("!!!! {}", &result);

    result.into()
}
#[proc_macro_attribute]
pub fn use_stateful_query(attr: TokenStream, tokens: TokenStream) -> TokenStream {
    todo!()
}
#[proc_macro_attribute]
pub fn use_stateless_query(attr: TokenStream, tokens: TokenStream) -> TokenStream {
    todo!()
}

