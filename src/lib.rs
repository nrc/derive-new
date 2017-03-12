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
    let result = match ast.body {
        syn::Body::Enum(ref variants) => new_for_enum(&ast, &variants),
        syn::Body::Struct(ref variant_data) => new_for_struct(&ast, &variant_data, None),
    };
    result.to_string().parse().expect("Couldn't parse string to tokens")
}


fn new_for_struct(ast: &syn::MacroInput, variant_data: &syn::VariantData,
                  variant: Option<&syn::Ident>) -> quote::Tokens
{
    match *variant_data {
        syn::VariantData::Struct(ref fields) => {
            new_impl(&ast, Some(&fields), true, variant)
        },
        syn::VariantData::Unit => {
            new_impl(&ast, None, false, variant)
        },
        syn::VariantData::Tuple(ref fields) => {
            new_impl(&ast, Some(&fields), false, variant)
        },
    }
}

fn new_for_enum(ast: &syn::MacroInput, variants: &[syn::Variant]) -> quote::Tokens {
    if variants.is_empty() {
        panic!("#[derive(new)] cannot be implemented for enums with zero variants");
    }
    let impls = variants.iter().map(|v| {
        if v.discriminant.is_some() {
            panic!("#[derive(new)] cannot be implemented for enums with discriminants");
        }
        new_for_struct(ast, &v.data, Some(&v.ident))
    });
    quote!(#(#impls)*)
}

fn new_impl(ast: &syn::MacroInput, fields: Option<&[syn::Field]>,
            named: bool, variant: Option<&syn::Ident>) -> quote::Tokens
{
    let name = &ast.ident;
    let unit = fields.is_none();
    let fields: Vec<_> = fields.unwrap_or(&[]).iter()
        .enumerate().map(|(i, f)| FieldExt::new(f, i, named)).collect();
    let args = fields.iter()
        .filter(|f| f.needs_arg()).map(|f| f.as_arg());
    let inits = fields.iter()
        .map(|f| f.as_init());
    let inits = if unit {
        quote!()
    } else if named {
        quote![{ #(#inits),* }]
    } else {
        quote![( #(#inits),* )]
    };
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();
    let (new, qual, doc) = match variant {
        None => (
            syn::Ident::new("new"),
            quote!(),
            format!("Constructs a new `{}`.", name),
        ),
        Some(ref variant) => (
            syn::Ident::new(format!("new_{}", to_snake_case(variant.as_ref()))),
            quote!(::#variant),
            format!("Constructs a new `{}::{}`.", name, variant),
        ),
    };
    let lint_attrs = collect_parent_lint_attrs(&ast.attrs);
    let lint_attrs = quote![#(#lint_attrs),*];
    quote! {
        impl #impl_generics #name #ty_generics #where_clause {
            #[doc = #doc]
            #lint_attrs
            pub fn #new(#(#args),*) -> Self {
                #name #qual #inits
            }
        }
    }
}

fn collect_parent_lint_attrs(attrs: &[syn::Attribute]) -> Vec<syn::Attribute> {
    fn is_lint(item: &syn::MetaItem) -> bool {
        if let syn::MetaItem::List(ref ident, _) = *item {
            match ident.as_ref() {
                "allow" | "deny" | "forbid" | "warn" => return true,
                _ => (),
            }
        }
        false
    }

    fn is_cfg_attr_lint(item: &syn::MetaItem) -> bool {
        if let syn::MetaItem::List(ref ident, ref items) = *item {
            if ident.as_ref() == "cfg_attr" && items.len() == 2 {
                if let syn::NestedMetaItem::MetaItem(ref item) = items[1] {
                    return is_lint(item);
                }
            }
        }
        false
    }

    attrs.iter().filter(|attr| {
        is_lint(&attr.value) || is_cfg_attr_lint(&attr.value)
    }).cloned().collect()
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

fn to_snake_case(s: &str) -> String {
    let (ch, next, mut acc): (Option<char>, Option<char>, String) =
        s.chars().fold((None, None, String::new()), |(prev, ch, mut acc), next| {
            if let Some(ch) = ch {
                if let Some(prev) = prev {
                    if ch.is_uppercase() {
                        if prev.is_lowercase() || prev.is_numeric() ||
                            (prev.is_uppercase() && next.is_lowercase())
                        {
                            acc.push('_');
                        }
                    }
                }
                acc.extend(ch.to_lowercase());
            }
            (ch, Some(next), acc)
        });
    if let Some(next) = next {
        if let Some(ch) = ch {
            if (ch.is_lowercase() || ch.is_numeric()) && next.is_uppercase() {
                acc.push('_');
            }
        }
        acc.extend(next.to_lowercase());
    }
    acc
}


#[test]
fn test_to_snake_case() {
    assert_eq!(to_snake_case(""), "");
    assert_eq!(to_snake_case("a"), "a");
    assert_eq!(to_snake_case("B"), "b");
    assert_eq!(to_snake_case("BC"), "bc");
    assert_eq!(to_snake_case("Bc"), "bc");
    assert_eq!(to_snake_case("bC"), "b_c");
    assert_eq!(to_snake_case("Fred"), "fred");
    assert_eq!(to_snake_case("CARGO"), "cargo");
    assert_eq!(to_snake_case("_Hello"), "_hello");
    assert_eq!(to_snake_case("QuxBaz"), "qux_baz");
    assert_eq!(to_snake_case("FreeBSD"), "free_bsd");
    assert_eq!(to_snake_case("specialK"), "special_k");
    assert_eq!(to_snake_case("hello1World"), "hello1_world");
    assert_eq!(to_snake_case("Keep_underscore"), "keep_underscore");
    assert_eq!(to_snake_case("ThisISNotADrill"), "this_is_not_a_drill");
}
