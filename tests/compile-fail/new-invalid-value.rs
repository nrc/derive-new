#[macro_use]
extern crate derive_new;

#[derive(new)]
//~^ ERROR proc-macro derive
//~^^ HELP Invalid expression in #[new]: `hello@world`
struct Foo {
    #[new(value = "hello@world")]
    x: i32,
}
