extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{self, parse_macro_input, Data, DataStruct, DeriveInput, Fields, FieldsNamed, Type};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree.
    let input = parse_macro_input!(input as DeriveInput);

    // Used in the quasi-quotation below as `#name`.
    let name = input.ident;

    let builder = format_ident!("{}Builder", name);

    let data: FieldsNamed = match input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(n),
            ..
        }) => n,
        other => unimplemented!("{:?}", other),
    };

    let fields = data.named.iter().filter_map(|field| {
        let ty = &field.ty;
        match &field.ident {
            Some(ident) => Some((ident, ty, inner_for_option(ty))),
            _ => None,
        }
    });

    let names = data.named.iter().filter_map(|field| match &field.ident {
        None => None,
        Some(ident) => Some((ident, inner_for_option(&field.ty))),
    });

    let initialize = names.clone().map(|(name, _)| quote! { #name: None });

    let extract = names.clone().map(|(name, option)| match option {
        None => quote! { #name: self.#name.clone()? },
        Some(_) => quote! { #name: self.#name.clone() },
    });

    let quoted_fields = fields.clone().map(|(name, ty, option)| match option {
        None => quote! { #name: Option<#ty> },
        Some(ty) => quote! { #name: Option<#ty> },
    });

    let methods = fields.clone().map(|(name, ty, option)| match option {
        None => quote! {
            pub fn #name(&mut self, value: #ty) -> &mut Self {
                self.#name = Some(value);
                self
            }
        },

        Some(ty) => quote! {
            pub fn #name(&mut self, value: #ty) -> &mut Self {
                self.#name = Some(value);
                self
            }
        },
    });

    let expanded = quote! {
        impl #name {
            fn builder() -> #builder {
                #builder {
                    #(
                        #initialize,
                    )*
                }
            }
        }

        struct #builder {
            #(
                #quoted_fields,
            )*
        }

        impl #builder {
            pub fn build(&self) -> Option<#name> {
                Some(#name {
                    #(
                        #extract,
                    )*
                })
            }

            #(
                #methods
            )*
        }
    };

    TokenStream::from(expanded)
}

fn inner_for_option(ty: &Type) -> Option<Type> {
    match ty {
        Type::Path(syn::TypePath {
            path: syn::Path { segments, .. },
            ..
        }) if segments[0].ident == "Option" => {
            let segment = &segments[0];

            match &segment.arguments {
                syn::PathArguments::AngleBracketed(generic) => {
                    match generic.args.first().unwrap() {
                        syn::GenericArgument::Type(ty) => Some(ty.clone()),
                        _ => None,
                    }
                }
                _ => None,
            }
        }

        _ => None,
    }
}
