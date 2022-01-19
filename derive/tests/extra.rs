use knuffel::span::Span;
use knuffel::traits::Decode;

#[derive(knuffel_derive::Decode, Debug, PartialEq)]
struct Child;

#[derive(knuffel_derive::Decode, Debug, PartialEq)]
#[knuffel(span_type=Span)]
struct NodeSpan {
    #[knuffel(span)]
    span: Span,
    #[knuffel(argument)]
    name: String,
    #[knuffel(children)]
    children: Vec<Child>,
}

fn parse<T: Decode<Span>>(text: &str) -> T {
    let mut nodes: Vec<T> = knuffel::parse("<test>", text).unwrap();
    assert_eq!(nodes.len(), 1);
    nodes.remove(0)
}

#[test]
fn parse_node_span() {
    assert_eq!(parse::<NodeSpan>(r#"node "hello""#),
               NodeSpan {
                   span: Span(0, 12),
                   name: "hello".into(),
                   children: Vec::new(),
               });
    assert_eq!(parse::<NodeSpan>(r#"   node  "hello"     "#),
               NodeSpan {
                   span: Span(3, 21),
                   name: "hello".into(),
                   children: Vec::new(),
               });
    assert_eq!(parse::<NodeSpan>(r#"   node  "hello";     "#),
               NodeSpan {
                   span: Span(3, 17),
                   name: "hello".into(),
                   children: Vec::new(),
               });
    assert_eq!(parse::<NodeSpan>(r#"   node  "hello"     {   child;   }"#),
               NodeSpan {
                   span: Span(3, 35),
                   name: "hello".into(),
                   children: vec![Child],
               });
}
