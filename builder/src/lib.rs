extern crate proc_macro;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DeriveInput, Fields, FieldsNamed, Ident};

#[proc_macro_derive(Builder)]
pub fn builder(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = input.ident;
    let builder_name = format_ident!("{}Builder", struct_name);
    let fields = builder_fields(&input.data);
    let fields_init = builder_fields_init(&input.data);
    let setters = builder_setters(&input.data);
    let build = builder_build(&input.data, &struct_name);
    let expanded = quote! {
        impl #struct_name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #fields_init
                }
            }
        }

        pub struct #builder_name {
            #fields
        }

        impl #builder_name {
            #setters

            pub fn build(&mut self) -> Result<#struct_name, Box<dyn std::error::Error>> {
                #build
            }
        }
    };
    proc_macro::TokenStream::from(expanded)
}

fn builder_fields(data: &Data) -> TokenStream {
    let fields = extract_fields(data);
    let option_wrapped = fields.named.iter().map(|f| {
        let ty = &f.ty;
        let ident = &f.ident;
        quote! {
            #ident: Option<#ty>
        }
    });
    quote! {
        #(#option_wrapped),*
    }
}

fn builder_fields_init(data: &Data) -> TokenStream {
    let fields = extract_fields(data);
    let value_none = fields.named.iter().map(|f| {
        let ident = &f.ident;
        quote! {
            #ident: None
        }
    });
    quote! {
        #(#value_none),*
    }
}

fn builder_setters(data: &Data) -> TokenStream {
    let fields = extract_fields(data);
    let setters = fields.named.iter().map(|f| {
        let ident = &f.ident;
        let ty = &f.ty;
        quote! {
            fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = Some(#ident);
                self
            }
        }
    });
    quote! {
        #(#setters)*
    }
}

fn builder_build(data: &Data, struct_name: &Ident) -> TokenStream {
    let fields = extract_fields(data);
    let set_check = fields.named.iter().map(|f| {
        let ident = &f.ident;
        let err = format!("field `{}` is not set.", ident.as_ref().unwrap());
        quote! {
            if self.#ident.is_none() {
                return Err(#err.into());
            }
        }
    });
    let build_values = fields.named.iter().map(|f| {
        let ident = &f.ident;
        quote! {
            #ident: self.#ident.clone().unwrap()
        }
    });
    quote! {
        #(#set_check)*

        Ok(#struct_name {
            #(#build_values),*
        })
    }
}

fn extract_fields(data: &Data) -> &FieldsNamed {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => fields,
            _ => panic!("all fields must be named."),
        },
        _ => panic!("struct expected, but got other item."),
    }
}
