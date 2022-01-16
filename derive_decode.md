The derive is the most interesting part of the `knuffel` libary.

# Overview

This trait and derive is used to decode a single node of the KDL document.

There are few things that derive can be implemented for:
1. Structure with named or unnamed fields. Most of the text here is about this
   case.
2. A single-field "new type" wrapper around such structure `Wrapper(Inner)`
   where `Inner` implements `Decode` (this is a tuple struct with single
   argument without annotations).
3. Unit struct
4. [Enum](#enums), where each variant corresponds to a specific node name

There are three kinds of things can fit structure fields that must be
annotated appropriately: [arguments](#arguments), [properties](#properties)
and [children](#children). Unlike in `serde` and similar projects,
non-annotated fields are not decoded from source and are filled with
[`std::default::Default`].

All annotations are enclosed by `#[knuffel(..)]` attribute.

Both arguments and properties can decode [scalars](#scalars).

If structure only has `child` and `children` fields (see [below](#children)) it
can be used as a root document (the output of [`knuffel::parse`]). Or root of the document can be `Vec<T> where T: Decode`.

Note: node name is usually not used in the structure decoding node, it's
matched either in parent or in an [enum](#enums).

# Arguments

Arguments are scalar values that are usually written on the same line with the
node name and are positional, i.e. they parsed and put into structure fields
in order.

The two Rust attributes to parse arguments are:
* `argument` -- to parse single argument
* `arguments` -- to parse sequence of arguments

Note: order of the structure fields matter. Fields marked as `arguments` can
be used only once and cannot be followed by `argument`.

For example, the following node:
```kdl
node "arg1" true 1 22 333
```
... can be parsed into the following structure:
```rust
#[derive(knuffel::Decode)]
struct MyNode {
    #[knuffel(argument)]
    first: String,
    #[knuffel(argument)]
    second: bool,
    #[knuffel(arguments)]
    numbers: Vec<u32>,
}
```

Arguments can be optional:
```rust
#[derive(knuffel::Decode)]
struct MyNode {
    #[knuffel(argument)]
    first: Option<String>,
    #[knuffel(argument)]
    second: Option<bool>,
}
```

In this case attribute may not exists:
```kdl
node "arg1"  // no `second` argument is okay
```
Or may be `null`:
```kdl
node null null
```

Note: due to limitations of the procedural macros in Rust, optional arguments
must use `Option` in this specific notation. Other variations like these:
```
use std::option::Option as Opt;
#[derive(knuffel::Decode)]
struct MyNode {
    #[knuffel(argument)]
    first: ::std::option::Option<String>,
    #[knuffel(argument)]
    second: Opt<bool>,
}
```
Do not work (they will always require `null` arguments).

The field marked as `arguments` can have any type that implements `FromIterator<T> where T: DecodeScalar`.

See [Scalars](#scalars) and [Common Attributes](#common-attributes) for more
information on decoding of values.

# Properties

Properties are scalar values that are usually written on the same line
prepended with name and equals `=` sign. They are parsed regardless of order,
although if the same argument is specified twice the latter value overrides
former.

The two Rust attributes to parse properties are:
* `property` -- to parse single argument
* `properties` -- to parse sequence of arguments

Note: order of the structure fields matter. Fields marked as `properties` can
be used only once and cannot be followed by `property`.

For example, the following node:
```kdl
node name="arg1" enabled=true a=1 b=2 c=3
```

Can be parsed into the following structure:
```rust
# use std::collections::HashMap;
#[derive(knuffel::Decode)]
struct MyNode {
    #[knuffel(property)]
    name: String,
    #[knuffel(property)]
    enabled: bool,
    #[knuffel(properties)]
    numbers: HashMap<String, u32>,
}
```

Properties can be optional:
```rust
#[derive(knuffel::Decode)]
struct MyNode {
    #[knuffel(argument)]
    name: Option<String>,
    #[knuffel(argument)]
    enabled: Option<bool>,
}
```

In this case property may not exists or may be set to `null`:
```kdl
node name=null
```

Note: due to limitations of the procedural macros in Rust, optional properties
must use `Option` in this specific notation. Other variations like this:
```rust
use std::option::Option as Opt;
#[derive(knuffel::Decode)]
struct MyNode {
    #[knuffel(argument)]
    first: ::std::option::Option<String>,
    #[knuffel(argument)]
    second: Opt<bool>,
}
```
Do not work (they will always require `property=null`).

By default, field name is renamed to use `kebab-case` in KDL file. So field
defined like this:
```rust
#[derive(knuffel::Decode)]
struct MyNode {
    #[knuffel(property)]
    plugin_name: String,
}
```
Parses the following:
```kdl
node plugin-name="my_plugin"
```

To rename a property in the source use `name=`:
```rust
#[derive(knuffel::Decode)]
struct MyNode {
    #[knuffel(property(name="pluginName"))]
    name: String,
}
```

The field marked as `properties` can have any type that implements
`FromIterator<(K, V)> where K: FromStr, V: DecodeScalar`.

See [Scalars](#scalars) and [Common Attributes](#common-attributes) for more
information on decoding of values.

# Scalars

There are additional attributes that define how scalar values are parsed:
* `str` -- uses [`FromStr`](std::str::FromStr) trait.
* `bytes` -- decodes binary strings, either by decoding `base64` if the `(base64)` type is specified in the source or by encoding string into `utf-8` if no type is specified. This is required since
* `default` -- described in [Common Attrbites](#common-attributes) section
  since it applies to nodes (non-scalar values) too.

All of them work on [properties](#properties) and [arguments](#arguments).

## Parsing Strings

The `str` marker is very useful for types coming from other libraries that
aren't supported by `knuffel` directly.

For example:
```rust
#[derive(knuffel::Decode)]
struct Server {
    #[knuffel(property, str)]
    listen: std::net::SocketAddr,
}
```
This will parse listening addresses that Rust stdlib supports, like this:
```kdl
server listen="127.0.0.1:8080"
```

## Parsing Bytes

Since in Rust sequence of ints and buffer of bytes cannot be distinguished on
the type level, there is a `bytes` marker that can be applied to parse scalar
as byte buffer.

For example:
```rust
#[derive(knuffel::Decode)]
struct Response {
    #[knuffel(argument, bytes)]
    body: Vec<u8>,
}
```

The value of `body` can be specified in two ways. Using `base64` string
(this requires `base64` feature enabled which is default):
```kdl
response (base64)"SGVsbG8gd29ybGQh"
```

While using base64 allows encoding any binary data, strings may also be used
and end up using utf-8 encoded in buffer. So the KDL above is equivalent to
the following:
```kdl
response "Hello world!"
```

The field don't have to be `Vec<u8>`, it may be any type that has
`TryInto<Vec<u8>>` (and hence also `Into<Vec<u8>>`) implementation. For
example
[`bstr::BString`](https://docs.rs/bstr/latest/bstr/struct.BString.html) and
[`bytes::Bytes`](https://docs.rs/bytes/latest/bytes/struct.Bytes.html) work too.


# Children

Nodes are fundamental blocks for data hierarchy in KDL. Here are some examples of nodes:
```kdl
node1 "x" "y"
(my_type)node2 prop="value" {
    node3 1
    node4 2
}
```
There are four nodes in this example. Nodes typically start with identifier
which is called node name. Similarly to scalars, nodes can be prepended by type
name in parenthesis. The nodes `node3` and `node4` are children nodes with
respect to `node2`. So when `node2` is decoded its `child` and `children`
directives are interpreted to match `node3` and `node4`.

The two Rust attributes to parse children are:
* `child` -- to parse single child
* `children` -- to parse sequence of children

For example the follwing KDL:
```kdl
node {
    version 1
    plugin "xxx"
    datum "yyy"
}
```
... can be parsed by into the following structures:
```rust
#[derive(knuffel::Decode)]
enum Setting {
    Plugin(#[knuffel(argument)] String),
    Datum(#[knuffel(argument)] String),
}
#[derive(knuffel::Decode)]
struct Version {
    #[knuffel(argument)]
    number: u32
}
#[derive(knuffel::Decode)]
struct MyNode {
    #[knuffel(child)]
    version: Version,
    #[knuffel(children)]
    settings: Vec<Setting>
}
```

There is another form of children which is `children(name="something")`, that
allows filtering nodes by name:
```rust
#[derive(knuffel::Decode)]
struct NamedNode {
    #[knuffel(argument)]
    name: u32
}
#[derive(knuffel::Decode)]
struct MyNode {
    #[knuffel(children(name="plugin"))]
    plugins: Vec<NamedNode>,
    #[knuffel(children(name="datum"))]
    data: Vec<NamedNode>,
}
```

Note: we use same node type for `plugin` and `datum` nodes. Generally nodes do
not match on the actual node names, it's the job of the parent node to sort
out their children into the right buckets. Also see [Enums](#enums).

## Boolean Child Fields

## Unwrapping


## Root Document

Any structure that has only fields marked as `child` and `children` or
unmarked ones, can be used as the root of the document.

For example, this structure can:
```rust
# #[derive(knuffel::Decode)]
# struct NamedNode { #[knuffel(argument)] name: u32 }
#[derive(knuffel::Decode)]
struct MyNode {
    #[knuffel(child, unwrap(argument))]
    version: u32,
    #[knuffel(children(name="plugin"))]
    plugins: Vec<NamedNode>,
    #[knuffel(children(name="datum"))]
    data: Vec<NamedNode>,
}
```
On the other hand this one can **not** because it contains a `property`:
```rust
# #[derive(knuffel::Decode)]
# struct NamedNode { #[knuffel(argument)] name: u32 }
#[derive(knuffel::Decode)]
struct MyNode {
    #[knuffel(property)]
    version: u32,
    #[knuffel(children(name="plugin"))]
    plugins: Vec<NamedNode>,
    #[knuffel(children(name="datum"))]
    data: Vec<NamedNode>,
}
```
Note: attributes in the `unwrap` have no influence on whether structure can be
used to decode document.

Technically [DecodeChildren](knuffel::traits::DecodeChildren) trait will be
implemented for the nodes that can be used as documents.


# Common Attributes

## Default

`default` attribute may be applied to any [arguments](#arguments),
[properties](#properties) or [children](#children).

There are two forms of it. Marker attribute:
```rust
#[derive(knuffel::Decode)]
struct MyNode {
    #[knuffel(property, default)]
    first: String,
}
```
Which means that `std::default::Default` should be used if field was not
filled otherwise (i.e. no such property encountered).

Another form is `default=value`:
```rust
#[derive(knuffel::Decode)]
struct MyNode {
    #[knuffel(property, default="unnamed".into())]
    name: String,
}
```
Any Rust expression can be used in this case.

Note, for optional properties `Some` should be included in the default value.
And for scalar values their value can be overriden by using `null`. The
definition like this:
```rust
#[derive(knuffel::Decode)]
struct MyNode {
    #[knuffel(property, default=Some("unnamed".into()))]
    name: Option<String>,
}
```
Parses these two nodes differently:
```kdl
node name=null
node
```
Will yield:
```rust
# struct MyNode { name: Option<String> }
let _ = vec![
    MyNode { name: None },
    MyNode { name: Some(String::from("unnamed")) },
];
```

# Flatten

# Enums

* `skip` attribute
