//! # A custom derive implementation for `#[derive(new)]`
//!
//! A `derive(new)` attribute creates a `new` constructor function for the annotated
//! type. That function takes an argument for each field in the type giving a
//! trivial constructor. This is useful since as your type evolves you can make the
//! constructor non-trivial (and add or remove fields) without changing client code
//! (i.e., without breaking backwards compatibility). It is also the most succinct
//! way to initialise a struct or an enum.
//!
//! Implementation uses macros 1.1 custom derive (which works in stable Rust from
//! 1.15 onwards).
//!
//! ## Examples
//!
//! Cargo.toml:
//!
//! ```toml
//! [dependencies]
//! derive-new = "0.5"
//! ```
//!
//! Include the macro:
//!
//! ```rust
//! use derive_new::new;
//!
//! fn main() {}
//! ```
//!
//! Generating constructor for a simple struct:
//!
//! ```rust
//! use derive_new::new;
//!
//! #[derive(new)]
//! struct Bar {
//!     a: i32,
//!     b: String,
//! }
//!
//! fn main() {
//!   let _ = Bar::new(42, "Hello".to_owned());
//! }
//! ```
//!
//! Default values can be specified either via `#[new(default)]` attribute which removes
//! the argument from the constructor and populates the field with `Default::default()`,
//! or via `#[new(value = "..")]` which initializes the field with a given expression:
//!
//! ```rust
//! use derive_new::new;
//!
//! #[derive(new)]
//! struct Foo {
//!     x: bool,
//!     #[new(value = "42")]
//!     y: i32,
//!     #[new(default)]
//!     z: Vec<String>,
//! }
//!
//! fn main() {
//!   let _ = Foo::new(true);
//! }
//! ```
//!
//! Generic types are supported; in particular, `PhantomData<T>` fields will be not
//! included in the argument list and will be initialized automatically:
//!
//! ```rust
//! use derive_new::new;
//!
//! use std::marker::PhantomData;
//!
//! #[derive(new)]
//! struct Generic<'a, T: Default, P> {
//!     x: &'a str,
//!     y: PhantomData<P>,
//!     #[new(default)]
//!     z: T,
//! }
//!
//! fn main() {
//!   let _ = Generic::<i32, u8>::new("Hello");
//! }
//! ```
//!
//! For enums, one constructor method is generated for each variant, with the type
//! name being converted to snake case; otherwise, all features supported for
//! structs work for enum variants as well:
//!
//! ```rust
//! use derive_new::new;
//!
//! #[derive(new)]
//! enum Enum {
//!     FirstVariant,
//!     SecondVariant(bool, #[new(default)] u8),
//!     ThirdVariant { x: i32, #[new(value = "vec![1]")] y: Vec<u8> }
//! }
//!
//! fn main() {
//!   let _ = Enum::new_first_variant();
//!   let _ = Enum::new_second_variant(true);
//!   let _ = Enum::new_third_variant(42);
//! }
//! ```
//! ### Setting Visibility for the Constructor
//!
//! By default, the generated constructor will be `pub`. However, you can control the visibility of the constructor using the `#[new(visibility = "...")]` attribute.
//!
//! #### Public Constructor (default)
//!
//! ```rust
//! use derive_new::new;
//!
//! #[derive(new)]
//! pub struct Bar {
//!     a: i32,
//!     b: String,
//! }
//!
//! fn main() {
//!   let _ = Bar::new(42, "Hello".to_owned());
//! }
//! ```
//!
//! #### Crate-Visible Constructor
//!
//! ```rust
//! use derive_new::new;
//!
//! #[derive(new)]
//! #[new(visibility = "pub(crate)")]
//! pub struct Bar {
//!     a: i32,
//!     b: String,
//! }
//!
//! fn main() {
//!   let _ = Bar::new(42, "Hello".to_owned());
//! }
//! ```
//!
//! #### Private Constructor
//!
//! ```rust
//! use derive_new::new;
//!
//! #[derive(new)]
//! #[new(visibility = "")]
//! pub struct Bar {
//!     a: i32,
//!     b: String,
//! }
//!
//! fn main() {
//!   // Bar::new is not accessible here as it is private
//!   let _ = Bar::new(42, "Hello".to_owned()); // This will cause a compile error
//! }
//! ```
#![crate_type = "proc-macro"]
#![recursion_limit = "192"]

extern crate proc_macro;
extern crate proc_macro2;
#[macro_use]
extern crate quote;
extern crate syn;

macro_rules! my_quote {
    ($($t:tt)*) => (quote_spanned!(proc_macro2::Span::call_site() => $($t)*))
}

fn path_to_string(path: &syn::Path) -> String {
    path.segments
        .iter()
        .map(|s| s.ident.to_string())
        .collect::<Vec<String>>()
        .join("::")
}

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use syn::{punctuated::Punctuated, Attribute, Lit, Token, Visibility};

#[proc_macro_derive(new, attributes(new))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast: syn::DeriveInput = syn::parse(input).expect("Couldn't parse item");
    let options = NewOptions::from_attributes(&ast.attrs);
    let result = match ast.data {
        syn::Data::Enum(ref e) => new_for_enum(&ast, e, &options),
        syn::Data::Struct(ref s) => new_for_struct(&ast, &s.fields, None, &options),
        syn::Data::Union(_) => panic!("doesn't work with unions yet"),
    };
    result.into()
}

fn new_for_struct(
    ast: &syn::DeriveInput,
    fields: &syn::Fields,
    variant: Option<&syn::Ident>,
    options: &NewOptions,
) -> proc_macro2::TokenStream {
    match *fields {
        syn::Fields::Named(ref fields) => {
            new_impl(ast, Some(&fields.named), true, variant, options)
        }
        syn::Fields::Unit => new_impl(ast, None, false, variant, options),
        syn::Fields::Unnamed(ref fields) => {
            new_impl(ast, Some(&fields.unnamed), false, variant, options)
        }
    }
}

fn new_for_enum(
    ast: &syn::DeriveInput,
    data: &syn::DataEnum,
    options: &NewOptions,
) -> proc_macro2::TokenStream {
    if data.variants.is_empty() {
        panic!("#[derive(new)] cannot be implemented for enums with zero variants");
    }
    let impls = data.variants.iter().map(|v| {
        if v.discriminant.is_some() {
            panic!("#[derive(new)] cannot be implemented for enums with discriminants");
        }
        new_for_struct(ast, &v.fields, Some(&v.ident), options)
    });
    my_quote!(#(#impls)*)
}

fn new_impl(
    ast: &syn::DeriveInput,
    fields: Option<&Punctuated<syn::Field, Token![,]>>,
    named: bool,
    variant: Option<&syn::Ident>,
    options: &NewOptions,
) -> proc_macro2::TokenStream {
    let name = &ast.ident;
    let unit = fields.is_none();
    let empty = Default::default();
    let fields: Vec<_> = fields
        .unwrap_or(&empty)
        .iter()
        .enumerate()
        .map(|(i, f)| FieldExt::new(f, i, named))
        .collect();
    let args = fields.iter().filter(|f| f.needs_arg()).map(|f| f.as_arg());
    let inits = fields.iter().map(|f| f.as_init());
    let inits = if unit {
        my_quote!()
    } else if named {
        my_quote![{ #(#inits),* }]
    } else {
        my_quote![( #(#inits),* )]
    };
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();
    let (mut new, qual, doc) = match variant {
        None => (
            syn::Ident::new("new", proc_macro2::Span::call_site()),
            my_quote!(),
            format!("Constructs a new `{}`.", name),
        ),
        Some(ref variant) => (
            syn::Ident::new(
                &format!("new_{}", to_snake_case(&variant.to_string())),
                proc_macro2::Span::call_site(),
            ),
            my_quote!(::#variant),
            format!("Constructs a new `{}::{}`.", name, variant),
        ),
    };
    new.set_span(proc_macro2::Span::call_site());
    let lint_attrs = collect_parent_lint_attrs(&ast.attrs);
    let lint_attrs = my_quote![#(#lint_attrs),*];
    let visibility = &options.visibility;
    my_quote! {
        impl #impl_generics #name #ty_generics #where_clause {
            #[doc = #doc]
            #lint_attrs
            #visibility fn #new(#(#args),*) -> Self {
                #name #qual #inits
            }
        }
    }
}

fn collect_parent_lint_attrs(attrs: &[syn::Attribute]) -> Vec<syn::Attribute> {
    fn is_lint(item: &syn::Meta) -> bool {
        if let syn::Meta::List(ref l) = *item {
            let path = &l.path;
            return path.is_ident("allow")
                || path.is_ident("deny")
                || path.is_ident("forbid")
                || path.is_ident("warn");
        }
        false
    }

    fn is_cfg_attr_lint(item: &syn::Meta) -> bool {
        if let syn::Meta::List(ref l) = *item {
            if l.path.is_ident("cfg_attr") {
                if let Ok(nested) =
                    l.parse_args_with(Punctuated::<syn::Meta, Token![,]>::parse_terminated)
                {
                    return nested.len() == 2 && is_lint(&nested[1]);
                }
            }
        }
        false
    }

    attrs
        .iter()
        .filter(|a| is_lint(&a.meta) || is_cfg_attr_lint(&a.meta))
        .cloned()
        .collect()
}

struct NewOptions {
    visibility: Option<syn::Visibility>,
}

impl NewOptions {
    fn from_attributes(attrs: &[Attribute]) -> Self {
        // Default visibility is public
        let mut visibility = Some(Visibility::Public(syn::token::Pub {
            span: proc_macro2::Span::call_site(),
        }));

        for attr in attrs {
            if attr.path().is_ident("new") {
                attr.parse_nested_meta(|meta| {
                    if meta.path.is_ident("visibility") {
                        let value: Lit = meta.value()?.parse()?;
                        if let Lit::Str(lit_str) = value {
                            // Parse the visibility string into a syn::Visibility type
                            let parsed_visibility: Visibility =
                                lit_str.parse().expect("Invalid visibility");
                            visibility = Some(parsed_visibility);
                        }
                        Ok(())
                    } else {
                        Err(meta.error("unsupported attribute"))
                    }
                })
                .unwrap_or(());
            }
        }

        NewOptions { visibility }
    }
}

enum FieldAttr {
    Default,
    Value(proc_macro2::TokenStream),
}

impl FieldAttr {
    pub fn as_tokens(&self) -> proc_macro2::TokenStream {
        match *self {
            FieldAttr::Default => my_quote!(::core::default::Default::default()),
            FieldAttr::Value(ref s) => my_quote!(#s),
        }
    }

    pub fn parse(attrs: &[syn::Attribute]) -> Option<FieldAttr> {
        let mut result = None;
        for attr in attrs.iter() {
            match attr.style {
                syn::AttrStyle::Outer => {}
                _ => continue,
            }
            let last_attr_path = attr
                .path()
                .segments
                .last()
                .expect("Expected at least one segment where #[segment[::segment*](..)]");
            if last_attr_path.ident != "new" {
                continue;
            }
            let list = match attr.meta {
                syn::Meta::List(ref l) => l,
                _ if attr.path().is_ident("new") => {
                    panic!("Invalid #[new] attribute, expected #[new(..)]")
                }
                _ => continue,
            };
            if result.is_some() {
                panic!("Expected at most one #[new] attribute");
            }
            for item in list
                .parse_args_with(Punctuated::<syn::Meta, Token![,]>::parse_terminated)
                .unwrap_or_else(|err| panic!("Invalid #[new] attribute: {}", err))
            {
                match item {
                    syn::Meta::Path(path) => {
                        if path.is_ident("default") {
                            result = Some(FieldAttr::Default);
                        } else {
                            panic!(
                                "Invalid #[new] attribute: #[new({})]",
                                path_to_string(&path)
                            );
                        }
                    }
                    syn::Meta::NameValue(kv) => {
                        if let syn::Expr::Lit(syn::ExprLit {
                            lit: syn::Lit::Str(ref s),
                            ..
                        }) = kv.value
                        {
                            if kv.path.is_ident("value") {
                                let tokens = lit_str_to_token_stream(s).ok().unwrap_or_else(|| {
                                    panic!("Invalid expression in #[new]: `{}`", s.value())
                                });
                                result = Some(FieldAttr::Value(tokens));
                            } else {
                                panic!(
                                    "Invalid #[new] attribute: #[new({} = ..)]",
                                    path_to_string(&kv.path)
                                );
                            }
                        } else {
                            panic!("Non-string literal value in #[new] attribute");
                        }
                    }
                    syn::Meta::List(l) => {
                        panic!(
                            "Invalid #[new] attribute: #[new({}(..))]",
                            path_to_string(&l.path)
                        );
                    }
                }
            }
        }
        result
    }
}

struct FieldExt<'a> {
    ty: &'a syn::Type,
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
                syn::Ident::new(&format!("f{}", idx), proc_macro2::Span::call_site())
            },
            named,
        }
    }

    pub fn has_attr(&self) -> bool {
        self.attr.is_some()
    }

    pub fn is_phantom_data(&self) -> bool {
        match *self.ty {
            syn::Type::Path(syn::TypePath {
                qself: None,
                ref path,
            }) => path
                .segments
                .last()
                .map(|x| x.ident == "PhantomData")
                .unwrap_or(false),
            _ => false,
        }
    }

    pub fn needs_arg(&self) -> bool {
        !self.has_attr() && !self.is_phantom_data()
    }

    pub fn as_arg(&self) -> proc_macro2::TokenStream {
        let f_name = &self.ident;
        let ty = &self.ty;
        my_quote!(#f_name: #ty)
    }

    pub fn as_init(&self) -> proc_macro2::TokenStream {
        let f_name = &self.ident;
        let init = if self.is_phantom_data() {
            my_quote!(::core::marker::PhantomData)
        } else {
            match self.attr {
                None => my_quote!(#f_name),
                Some(ref attr) => attr.as_tokens(),
            }
        };
        if self.named {
            my_quote!(#f_name: #init)
        } else {
            my_quote!(#init)
        }
    }
}

fn lit_str_to_token_stream(s: &syn::LitStr) -> Result<TokenStream2, proc_macro2::LexError> {
    let code = s.value();
    let ts: TokenStream2 = code.parse()?;
    Ok(set_ts_span_recursive(ts, &s.span()))
}

fn set_ts_span_recursive(ts: TokenStream2, span: &proc_macro2::Span) -> TokenStream2 {
    ts.into_iter()
        .map(|mut tt| {
            tt.set_span(*span);
            if let proc_macro2::TokenTree::Group(group) = &mut tt {
                let stream = set_ts_span_recursive(group.stream(), span);
                *group = proc_macro2::Group::new(group.delimiter(), stream);
            }
            tt
        })
        .collect()
}

fn to_snake_case(s: &str) -> String {
    let (ch, next, mut acc): (Option<char>, Option<char>, String) =
        s.chars()
            .fold((None, None, String::new()), |(prev, ch, mut acc), next| {
                if let Some(ch) = ch {
                    if let Some(prev) = prev {
                        if ch.is_uppercase()
                            && (prev.is_lowercase()
                                || prev.is_numeric()
                                || (prev.is_uppercase() && next.is_lowercase()))
                        {
                            acc.push('_');
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
