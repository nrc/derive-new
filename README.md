# A custom derive implementation for `#[derive(new)]`

Implementation is in derive-new. Tests and examples are in derive-new-test.

Example:

```
#[macro_use]
extern crate derive_new;

#[derive(new)]
struct Bar {
    x: i32,
    y: String,
}

fn main() {
    let _ = Bar::new(42, "Hello".to_owned());
}
```

Implementation uses macros 1.1 custom derive.
