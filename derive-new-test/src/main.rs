#![feature(rustc_macro)]

#[macro_use]
extern crate derive_new;

#[derive(new)]
struct Foo {}

#[derive(new)]
struct Bar {
    x: i32,
    y: String,
}

#[derive(new)]
struct Intersection<'scene> {
    object: &'scene Bar,
    normal: Foo,
    point: Foo,
    t: f64,
}

// TODO to test
// generics
// where clause
// no brace struct
// tuple struct - should error out?

fn main() {
    let _ = Foo::new();
    let b = Bar::new(42, "Hello".to_owned());

    let _ = Intersection::new(&b, Foo::new(), Foo::new(), 42.0);
}
