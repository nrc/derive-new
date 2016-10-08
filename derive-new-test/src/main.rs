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

// TODO to test
// generics
// where clause
// no brace struct
// tuple struct - should error out?

fn main() {
    let _ = Foo::new();
    let _ = Bar::new(42, "Hello".to_owned());
}
