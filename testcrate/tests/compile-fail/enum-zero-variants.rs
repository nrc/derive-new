#[macro_use]
extern crate derive_new;

#[derive(new)]
//~^ ERROR proc-macro derive
//~^^ HELP #[derive(new)] cannot be implemented for enums with zero variants
enum Enum {}

fn main() {}
