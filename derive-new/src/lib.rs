#![crate_type = "proc-macro"]
#![feature(proc_macro, proc_macro_lib)]

extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate quote;

use proc_macro::TokenStream;

#[proc_macro_derive(new)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input: String = input.to_string();

    let ast = syn::parse_macro_input(&input).expect("Couldn't parse item");

    let result = new_for_struct(ast);

    format!("{}\n{}", input, result).parse().expect("couldn't parse string to tokens")
}

fn new_for_struct(ast: syn::MacroInput) -> quote::Tokens {
    let name = &ast.ident;
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();

    match ast.body {
        syn::Body::Struct(syn::VariantData::Struct(ref fields)) => {
            let args = fields.iter().map(|f| {
                let f_name = &f.ident;
                let ty = &f.ty;
                quote!(#f_name: #ty)
            });
            let inits = fields.iter().map(|f| {
                let f_name = &f.ident;
                quote!(#f_name: #f_name)
            });

            quote! {
                impl #impl_generics #name #ty_generics #where_clause {
                    pub fn new(#(args),*) -> Self {
                        #name { #(inits),* }
                    }
                }
            }
        },
        syn::Body::Struct(syn::VariantData::Unit) => {
            quote! {
                impl #impl_generics #name #ty_generics #where_clause {
                    pub fn new() -> Self {
                        #name
                    }
                }
            }
        },
        syn::Body::Struct(syn::VariantData::Tuple(ref fields)) => {
            let field = &fields[0];
            let ty = &field.ty;

            quote! {
                impl #impl_generics #name #ty_generics #where_clause {
                    pub fn new(value: #ty) -> Self {
                        #name(value)
                    }
                }
            }
        },
        _ => panic!("#[derive(new)] can only be used with structs"),
    }
}
