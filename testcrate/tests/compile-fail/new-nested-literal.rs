#[macro_use]
extern crate derive_new;

#[derive(new)]
//~^ ERROR proc-macro derive
//~^^ HELP Invalid #[new] attribute: literal value in #[new(..)]
struct Foo {
    #[new("foo")]
    x: i32,
}
