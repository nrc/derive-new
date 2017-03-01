#![crate_type = "proc-macro"]

#![recursion_limit = "192"]

extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate quote;

use proc_macro::TokenStream;

#[proc_macro_derive(new, attributes(new))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input: String = input.to_string();
    let ast = syn::parse_macro_input(&input).expect("Couldn't parse item");
    let result = new_for_struct(ast);
    result.to_string().parse().expect("Couldn't parse string to tokens")
}

enum FieldAttr {
    Default,
    Value(syn::Expr),
}

impl FieldAttr {
    pub fn to_tokens(&self) -> quote::Tokens {
        match *self {
            FieldAttr::Default => quote!(::std::default::Default::default()),
            FieldAttr::Value(ref s) => quote!(#s),
        }
    }
}

fn parse_attrs(attrs: &[syn::Attribute]) -> Option<FieldAttr> {
    use syn::{MetaItem, NestedMetaItem, AttrStyle};

    let mut result = None;
    for attr in attrs.iter() {
        if attr.style == AttrStyle::Outer && !attr.is_sugared_doc && attr.value.name() == "new" {
            if let MetaItem::List(_, ref items) = attr.value {
                for item in items {
                    if result.is_some() {
                        panic!("Expected at most one #[new] attribute");
                    }
                    match *item {
                        NestedMetaItem::MetaItem(MetaItem::Word(ref ident)) => {
                            if ident.as_ref() == "default" {
                                result = Some(FieldAttr::Default);
                            } else {
                                panic!("Invalid #[new] attribute: #[new({})]", ident);
                            }
                        },
                        NestedMetaItem::MetaItem(MetaItem::NameValue(ref ident, ref lit)) => {
                            if let syn::Lit::Str(ref s, _) = *lit {
                                if ident.as_ref() == "value" {
                                    let expr = syn::parse_expr(s.as_ref()).ok()
                                        .expect(&format!("Invalid expression in #[new]: `{}`", s));
                                    result = Some(FieldAttr::Value(expr));
                                } else {
                                    panic!("Invalid #[new] attribute: #[new({} = ..)]", ident);
                                }
                            } else {
                                panic!("Non-string literal value in #[new] attribute");
                            }
                        },
                        NestedMetaItem::MetaItem(MetaItem::List(ref ident, _)) => {
                            panic!("Invalid #[new] attribute: #[new({}(..))]", ident);
                        },
                        NestedMetaItem::Literal(_) => {
                            panic!("Invalid #[new] attribute: literal value in #[new(..)]");
                        }
                    }
                }

            } else {
                panic!("Invalid #[new] attribute, expected #[new(..)]");
            }
        }
    }
    result
}

fn is_phantom_data(ty: &syn::Ty) -> bool {
    match *ty {
        syn::Ty::Path(None, ref path) => {
            path.segments.as_slice().last()
                .map(|x| x.ident.as_ref() == "PhantomData").unwrap_or(false)
        },
        _ => false,
    }
}

fn new_for_struct(ast: syn::MacroInput) -> quote::Tokens {
    let name = &ast.ident;
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();
    let doc_comment = format!("Constructs a new `{}`.", name);

    match ast.body {
        syn::Body::Struct(syn::VariantData::Struct(ref fields)) => {
            let attrs = fields.iter()
                .map(|f| parse_attrs(&f.attrs))
                .collect::<Vec<_>>();
            let args = fields.iter()
                .zip(attrs.iter())
                .filter(|&(_, a)| a.is_none())
                .map(|(f, _)| f)
                .filter(|f| !is_phantom_data(&f.ty))
                .map(|f| {
                    let f_name = &f.ident;
                    let ty = &f.ty;
                    quote!(#f_name: #ty)
                });
            let inits = fields.iter()
                .zip(attrs.iter())
                .map(|(f, ref a)| {
                    let f_name = &f.ident;
                    let init = if is_phantom_data(&f.ty) {
                        quote!(::std::marker::PhantomData)
                    } else {
                        match **a {
                            None => quote!(#f_name),
                            Some(ref a) => a.to_tokens(),
                        }
                    };
                    quote!(#f_name: #init)
                });
            quote! {
                impl #impl_generics #name #ty_generics #where_clause {
                    #[doc = #doc_comment]
                    pub fn new(#(#args),*) -> Self {
                        #name { #(#inits),* }
                    }
                }
            }
        },
        syn::Body::Struct(syn::VariantData::Unit) => {
            quote! {
                impl #impl_generics #name #ty_generics #where_clause {
                    #[doc = #doc_comment]
                    pub fn new() -> Self {
                        #name
                    }
                }
            }
        },
        syn::Body::Struct(syn::VariantData::Tuple(ref fields)) => {
            let (args, inits): (Vec<_>, Vec<_>) = fields.iter().enumerate().map(|(i, f)| {
                let f_name = syn::Ident::new(format!("value{}", i));
                let ty = &f.ty;
                (quote!(#f_name: #ty), f_name)
            }).unzip();

            quote! {
                impl #impl_generics #name #ty_generics #where_clause {
                    #[doc = #doc_comment]
                    pub fn new(#(#args),*) -> Self {
                        #name(#(#inits),*)
                    }
                }
            }
        },
        _ => panic!("#[derive(new)] can only be used with structs"),
    }
}
