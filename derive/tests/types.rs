use std::path::PathBuf;

use knuffel::{span::Span};
use knuffel::traits::DecodeChildren;


#[derive(knuffel_derive::Decode, Debug, PartialEq)]
struct Scalars {
    #[knuffel(child, unwrap(argument))]
    str: String,
    #[knuffel(child, unwrap(argument))]
    u64: u64,
    #[knuffel(child, unwrap(argument))]
    f64: f64,
    #[knuffel(child, unwrap(argument))]
    path: PathBuf,
    #[knuffel(child, unwrap(argument))]
    boolean: bool,
}

fn parse<T: DecodeChildren<Span>>(text: &str) -> T {
    knuffel::parse("<test>", text).unwrap()
}


#[test]
fn parse_enum() {
    assert_eq!(
        parse::<Scalars>(r#"
            str "hello"
            u64 1234
            f64 1.234
            path "/hello/world"
            boolean true
        "#),
        Scalars {
            str: "hello".into(),
            u64: 1234,
            f64: 1.234
            path: PathBuf::from("/hello/world"),
            boolean: true,
        });
}
