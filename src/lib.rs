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

fn new_for_struct(ast: syn::MacroInput) -> quote::Tokens {
    match ast.body {
        syn::Body::Struct(syn::VariantData::Struct(ref fields)) => {
            new_impl(&ast, Some(&fields), true)
        },
        syn::Body::Struct(syn::VariantData::Unit) => {
            new_impl(&ast, None, false)
        },
        syn::Body::Struct(syn::VariantData::Tuple(ref fields)) => {
            new_impl(&ast, Some(&fields), false)
        },
        _ => panic!("#[derive(new)] can only be used with structs"),
    }
}

fn new_impl(ast: &syn::MacroInput, fields: Option<&[syn::Field]>, named: bool) -> quote::Tokens {
    let name = &ast.ident;
    let unit = fields.is_none();
    let fields: Vec<_> = fields.unwrap_or(&[]).iter()
        .enumerate().map(|(i, f)| FieldExt::new(f, i, named)).collect();
    let args = fields.iter()
        .filter(|f| f.needs_arg()).map(|f| f.as_arg());
    let inits = fields.iter()
        .map(|f| f.as_init()).collect::<Vec<_>>();
    let inits = if unit {
        quote!()
    } else if named {
        quote![{ #(#inits),* }]
    } else {
        quote![( #(#inits),* )]
    };
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();
    let doc_comment = format!("Constructs a new `{}`.", name);
    quote! {
        impl #impl_generics #name #ty_generics #where_clause {
            #[doc = #doc_comment]
            pub fn new(#(#args),*) -> Self {
                #name #inits
            }
        }
    }
}

enum FieldAttr {
    Default,
    Value(syn::Expr),
}

impl FieldAttr {
    pub fn as_tokens(&self) -> quote::Tokens {
        match *self {
            FieldAttr::Default => quote!(::std::default::Default::default()),
            FieldAttr::Value(ref s) => quote!(#s),
        }
    }

    pub fn parse(attrs: &[syn::Attribute]) -> Option<FieldAttr> {
        use syn::{MetaItem, NestedMetaItem, AttrStyle};

        let mut result = None;
        for attr in attrs.iter() {
            if attr.style == AttrStyle::Outer && attr.value.name() == "new" {
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
                                            .expect(&format!(
                                                "Invalid expression in #[new]: `{}`", s));
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

}

struct FieldExt<'a> {
    ty: &'a syn::Ty,
    attr: Option<FieldAttr>,
    ident: syn::Ident,
    named: bool,
}

impl<'a> FieldExt<'a> {
    pub fn new(field: &'a syn::Field, idx: usize, named: bool) -> FieldExt<'a> {
        FieldExt {
            ty: &field.ty,
            attr: FieldAttr::parse(&field.attrs),
            ident: if named {
                field.ident.clone().unwrap()
            } else {
                syn::Ident::new(format!("f{}", idx))
            },
            named: named,
        }
    }

    pub fn has_attr(&self) -> bool {
        self.attr.is_some()
    }

    pub fn is_phantom_data(&self) -> bool {
        match *self.ty {
            syn::Ty::Path(None, ref path) => {
                path.segments.as_slice().last()
                    .map(|x| x.ident.as_ref() == "PhantomData").unwrap_or(false)
            },
            _ => false,
        }
    }

    pub fn needs_arg(&self) -> bool {
        !self.has_attr() && !self.is_phantom_data()
    }

    pub fn as_arg(&self) -> quote::Tokens {
        let f_name = &self.ident;
        let ty = &self.ty;
        quote!(#f_name: #ty)
    }

    pub fn as_init(&self) -> quote::Tokens {
        let f_name = &self.ident;
        let init = if self.is_phantom_data() {
            quote!(::std::marker::PhantomData)
        } else {
            match self.attr {
                None => quote!(#f_name),
                Some(ref attr) => attr.as_tokens(),
            }
        };
        if self.named {
            quote!(#f_name: #init)
        } else {
            quote!(#init)
        }
    }
}
