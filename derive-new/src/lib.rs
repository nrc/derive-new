#![crate_type = "rustc-macro"]
#![feature(rustc_macro, rustc_macro_lib)]

extern crate rustc_macro;
extern crate syn;
#[macro_use]
extern crate quote;

use rustc_macro::TokenStream;

#[rustc_macro_derive(new)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input: String = input.to_string();

    let ast = syn::parse_macro_input(&input).expect("Couldn't parse item");

    let result = new_for_struct(ast);

    format!("{}\n{}", input, result).parse().expect("couldn't parse string to tokens")
}

fn new_for_struct(ast: syn::MacroInput) -> quote::Tokens {
    let fields = match ast.body {
        syn::Body::Struct(syn::VariantData::Struct(ref fields)) => fields,
        _ => panic!("#[derive(new)] can only be used with braced structs"),
    };

    let name = &ast.ident;
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();
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
}


#[cfg(test)]
mod tests {
    #[test]
    fn todo() {
    }
}
