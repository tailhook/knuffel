use std::fmt;

use knuffel::{raw_parse, Decode};
use knuffel::span::Span;


#[derive(knuffel::DecodeScalar, Debug, PartialEq)]
enum SomeScalar {
    First,
    AnotherOption,
}

#[derive(knuffel::Decode, Debug, PartialEq)]
struct Item {
    #[knuffel(argument)]
    value: SomeScalar,
}


fn parse<T: Decode<Span>>(text: &str) -> T {
    let doc = raw_parse(text).unwrap();
    T::decode_node(&doc.nodes[0]).unwrap()
}

fn parse_err<T: Decode<Span>+fmt::Debug>(text: &str) -> String {
    let doc = raw_parse(text).unwrap();
    T::decode_node(&doc.nodes[0]).unwrap_err().to_string()
}

#[test]
fn parse_some_scalar() {
    assert_eq!(parse::<Item>(r#"node "first""#),
               Item { value: SomeScalar::First } );
    assert_eq!(parse::<Item>(r#"node "another-option""#),
               Item { value: SomeScalar::AnotherOption } );
    assert_eq!(parse_err::<Item>(r#"node "test""#),
        "5..11: expected one of `first`, `another-option`");
}
