#![feature(proc_macro)]

#[macro_use]
extern crate derive_new;

#[derive(new)]
struct Foo {}

#[derive(new)]
struct Baz {}

#[derive(new)]
struct Bar {
    x: i32,
    y: String,
}

#[derive(new)]
struct Intersection<'scene> {
    object: &'scene Bar,
    normal: Foo,
    point: Foo,
    t: f64,
}

#[derive(new)]
struct Qux<T, U: ::std::fmt::Debug> {
    f1: T,
    f2: Vec<U>,
    f3: i32,
}

// TODO to test
// where clause

fn main() {
    let _ = Foo::new();
    let _ = Baz::new();
    let b = Bar::new(42, "Hello".to_owned());

    let _ = Intersection::new(&b, Foo::new(), Foo::new(), 42.0);

    let _ = Qux::new("Hello!", Vec::<String>::new(), 42);
    let _: Qux<&'static str, String> = Qux::new("Hello!", Vec::<String>::new(), 42);
    let _ = Qux::<_, String>::new("Hello!", vec![], 42);
}
