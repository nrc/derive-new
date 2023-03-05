#[macro_use]
extern crate derive_new;

#[derive(new)]
//~^ ERROR proc-macro derive
//~^^ HELP Invalid #[new] attribute: #[new(foo)]
struct Foo {
    #[new(foo)]
    x: i32,
}

fn main() {}