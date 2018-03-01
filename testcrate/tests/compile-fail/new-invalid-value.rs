#[macro_use]
extern crate derive_new;

#[derive(new)]
//~^ ERROR produced unparseable tokens
//~^^ ERROR expected one of
struct Foo {
    #[new(value = "hello@world")]
    x: i32,
}
