#[macro_use]
extern crate derive_new;

#[derive(new)]
//~^ ERROR proc-macro derive
//~^^ HELP Non-string literal value in #[new] attribute
struct Foo {
    #[new(value = false)]
    x: i32,
}
