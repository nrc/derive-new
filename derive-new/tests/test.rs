#[macro_use]
extern crate derive_new;

use std::fmt::Debug;

/// A struct with no fields.
#[derive(new, PartialEq, Debug)]
pub struct Foo {}

#[test]
fn test_empty_struct() {
    let x = Foo::new();
    assert_eq!(x, Foo {});
}

/// A unit struct.
#[derive(new, PartialEq, Debug)]
pub struct Baz;

#[test]
fn test_unit_struct() {
    let x = Baz::new();
    assert_eq!(x, Baz);
}

/// A struct with fields.
#[derive(new, PartialEq, Debug)]
pub struct Bar {
    pub x: i32,
    pub y: String,
}

#[test]
fn test_simple_struct() {
    let x = Bar::new(42, "Hello".to_owned());
    assert_eq!(x, Bar { x: 42, y: "Hello".to_owned() });
}

/// A struct with a lifetime parameter.
#[derive(new, PartialEq, Debug)]
pub struct Intersection<'scene> {
    pub object: &'scene Bar,
    pub normal: Foo,
    pub point: Foo,
    pub t: f64,
}

#[test]
fn test_struct_with_lifetime() {
    let b = Bar::new(42, "Hello".to_owned());
    let x = Intersection::new(&b, Foo::new(), Foo::new(), 42.0);
    assert_eq!(x, Intersection { object: &b, normal: Foo {}, point: Foo {}, t: 42.0 });
}

/// A struct with generics and bounds.
#[derive(new, PartialEq, Debug)]
pub struct Qux<T: Debug + PartialEq, U: Debug + PartialEq> {
    pub f1: T,
    pub f2: Vec<U>,
    pub f3: i32,
}

#[test]
fn test_struct_with_bounds() {
    let x = Qux::new("Hello!", Vec::<String>::new(), 42);
    assert_eq!(x, Qux { f1: "Hello!", f2: vec![], f3: 42 });

    let x: Qux<&'static str, String> = Qux::new("Hello!", Vec::<String>::new(), 42);
    assert_eq!(x, Qux { f1: "Hello!", f2: vec![], f3: 42 });

    let x = Qux::<_, String>::new("Hello!", vec![], 42);
    assert_eq!(x, Qux { f1: "Hello!", f2: vec![], f3: 42 });
}

/// A struct with a lifetime parameter, generics and bounds.
#[derive(new, PartialEq, Debug)]
pub struct FooBar<'a, T, U>
    where T: 'a + PartialEq + Debug,
          U: Sized + Send + 'a + PartialEq + Debug
{
    pub f1: Box<T>,
    pub f2: Vec<&'a U>,
    pub f3: i32,
}

#[test]
fn test_struct_lifetime_bounds() {
    let a = 42;
    let x = FooBar::new(Box::new("Hello".to_owned()), vec![&a], 42);
    assert_eq!(x, FooBar { f1: Box::new("Hello".to_owned()), f2: vec![&a], f3: 42 });
}

/// A tuple struct.
#[derive(new, PartialEq, Debug)]
pub struct Tuple(pub i32, pub i32);

#[test]
fn test_simple_tuple_struct() {
    let x = Tuple::new(5, 6);
    assert_eq!(x, Tuple(5, 6));
}

/// A tuple struct with a lifetime parameter.
#[derive(new, PartialEq, Debug)]
pub struct TupleWithLifetime<'a>(pub &'a str);

#[test]
fn test_tuple_struct_lifetime() {
    let x = TupleWithLifetime::new("Hello");
    assert_eq!(x, TupleWithLifetime("Hello"));
}
