#[macro_use]
extern crate derive_new;

#[derive(new)]
//~^ ERROR proc-macro derive
//~^^ HELP #[derive(new)] cannot be implemented for enums with discriminants
enum Enum {
    Foo,
    Bar = 42,
}

fn main() {}
