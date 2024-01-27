use proc_macro::TokenStream;
use quote::{format_ident, quote};
use regex::Regex;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    Attribute, Data, DeriveInput, GenericArgument, Ident, ImplItem, ItemImpl, Meta, Path,
    PathArguments, Token, Type, TypeParamBound,
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
    let DefMutationInput { target_ident } = parse_macro_input!(input as DefMutationInput);

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

struct DefMutationInput {
    target_ident: syn::Ident,
}
impl Parse for DefMutationInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let target_ident = input.parse()?;
        Ok(DefMutationInput { target_ident })
    }
}
