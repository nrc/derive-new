#[macro_use]
extern crate derive_new;

#[derive(new)]
//~^ ERROR produced unparsable tokens
struct Foo {
    #[new(value = "hello@world")]
//~^ ERROR expected one of
    x: i32,
}
