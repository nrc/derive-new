#[macro_use]
extern crate derive_new;

#[derive(new)]
//~^ ERROR produced unparseable tokens
struct Foo {
    #[new(value = "hello@world")]
//~^ ERROR expected one of
    x: i32,
}

fn main() {}