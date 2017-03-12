#[macro_use]
extern crate derive_new;

#[derive(new)]
//~^ ERROR proc-macro derive
//~^^ HELP Expected at most one #[new] attribute
struct Foo {
    x: i32,
    #[new(default)]
    #[new(value = "42")]
    y: i32,
}
