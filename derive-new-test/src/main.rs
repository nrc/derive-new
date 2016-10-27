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

#[derive(new)]
struct FooBar<'a, T, U>
    where T: 'a,
          U: Sized + Send + 'a
{
    f1: Box<T>,
    f2: Vec<&'a U>,
    f3: i32,
}

#[derive(new)]
struct Tuple(String);

#[derive(new)]
struct TupleWithLifetime<'a>(&'a str);

fn main() {
    let _ = Foo::new();
    let _ = Baz::new();
    let b = Bar::new(42, "Hello".to_owned());

    let _ = Intersection::new(&b, Foo::new(), Foo::new(), 42.0);

    let _ = Qux::new("Hello!", Vec::<String>::new(), 42);
    let _: Qux<&'static str, String> = Qux::new("Hello!", Vec::<String>::new(), 42);
    let _ = Qux::<_, String>::new("Hello!", vec![], 42);

    let _ = FooBar::new(Box::new("Hello".to_owned()), vec![&42], 42);

    let _ = Tuple::new("Hello".to_owned());

    let _ = TupleWithLifetime::new("Hello");
}
