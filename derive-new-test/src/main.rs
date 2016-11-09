#![allow(dead_code)]
#![deny(missing_docs)]
#![feature(proc_macro)]

//! Test crate for derive-new. If it compiles, then it's tested.

#[macro_use]
extern crate derive_new;

/// A struct with no fields.
#[derive(new)]
pub struct Foo {}

/// A unit struct.
#[derive(new)]
pub struct Baz;

/// A struct with fields.
#[derive(new)]
pub struct Bar {
    x: i32,
    y: String,
}

/// A struct with a lifetime parameter.
#[derive(new)]
pub struct Intersection<'scene> {
    object: &'scene Bar,
    normal: Foo,
    point: Foo,
    t: f64,
}

/// A struct with generics and bounds.
#[derive(new)]
pub struct Qux<T, U: ::std::fmt::Debug> {
    f1: T,
    f2: Vec<U>,
    f3: i32,
}

/// A struct with a lifetime parameter, generics and bounds.
#[derive(new)]
pub struct FooBar<'a, T, U>
    where T: 'a,
          U: Sized + Send + 'a
{
    f1: Box<T>,
    f2: Vec<&'a U>,
    f3: i32,
}

/// A tuple struct.
#[derive(new)]
pub struct Tuple(i32, i32);

/// A tuple struct with a lifetime parameter.
#[derive(new)]
pub struct TupleWithLifetime<'a>(&'a str);

fn main() {
    let _ = Foo::new();
    let _ = Baz::new();
    let b = Bar::new(42, "Hello".to_owned());

    let _ = Intersection::new(&b, Foo::new(), Foo::new(), 42.0);

    let _ = Qux::new("Hello!", Vec::<String>::new(), 42);
    let _: Qux<&'static str, String> = Qux::new("Hello!", Vec::<String>::new(), 42);
    let _ = Qux::<_, String>::new("Hello!", vec![], 42);

    let _ = FooBar::new(Box::new("Hello".to_owned()), vec![&42], 42);

    let _ = Tuple::new(5, 6);

    let _ = TupleWithLifetime::new("Hello");
}
