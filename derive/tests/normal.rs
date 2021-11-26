use knuffel::{Decode, span::Span, raw_parse};

#[derive(knuffel_derive::Decode, Debug, PartialEq)]
struct Arg1 {
    #[knuffel(argument)]
    name: String,
}

fn parse<T: Decode<Span>>(text: &str) -> T {
    let doc = raw_parse(text).unwrap();
    T::decode_node(&doc.nodes[0]).unwrap()
}

#[test]
fn parse_arg1() {
    assert_eq!(parse::<Arg1>(r#"node "hello""#),
               Arg1 { name: "hello".into() } );
}
