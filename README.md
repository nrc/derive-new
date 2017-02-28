# A custom derive implementation for `#[derive(new)]`

A `derive(new)` attribute creates a `new` constructor function for the annotated
type. That function takes an argument for each field in the type giving a
trivial constructor. This is useful since as your type evolves you can make the
constructor non-trivial (and add or remove fields) without changing client code
(i.e., without breaking backwards compatibility). It is also the most succinct
way to initialise a struct.

## Example

Cargo.toml:

```toml
[dependencies]
derive-new = "0.4"
```

Rust code:

```rust
#[macro_use]
extern crate derive_new;

#[derive(new)]
struct Bar {
    a: i32,
    b: String,
    #[new(default)] c: Vec<u32>,
    #[new(value = "false")] d: bool,
}

fn main() {
    let _ = Bar::new(42, "Hello".to_owned());
}
```

Implementation is in derive-new. Tests and examples are in derive-new-test.

Implementation uses macros 1.1 custom derive (which works in stable Rust from
1.15 onwards).
