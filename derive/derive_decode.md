The derive is the most interesting part of the `knuffel` libary.

# Overview

This trait and derive is used to decode a single node of the KDL document.

There are few things that derive can be implemented for:
1. Structure with named or unnamed fields. Most of the text here is about this
   case.
2. A single-field [new type] wrapper around such structure `Wrapper(Inner)`
   where `Inner` implements `Decode` (this is a tuple struct with single
   argument without annotations).
3. Unit struct
4. [Enum](#enums), where each variant corresponds to a specific node name

[new type]: https://doc.rust-lang.org/rust-by-example/generics/new_types.html

There are three kinds of things can fit structure fields that must be
annotated appropriately: [arguments](#arguments), [properties](#properties)
and [children](#children). Unlike in `serde` and similar projects,
non-annotated fields are not decoded from source and are filled with
[`std::default::Default`].

All annotations are enclosed by `#[knuffel(..)]` attribute.

Both arguments and properties can decode [scalars](#scalars).

If structure only has `child` and `children` fields (see [below](#children)) it
can be used as a root document (the output of [`knuffel::parse`]). Or root of
the document can be `Vec<T> where T: Decode`.

[`knuffel::parse`]: fn.parse.html

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
    #[knuffel(property)]
    name: Option<String>,
    #[knuffel(property)]
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
    #[knuffel(property)]
    name: ::std::option::Option<String>,
    #[knuffel(property)]
    enabled: Opt<bool>,
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

Nodes are fundamental blocks for data hierarchy in KDL. Here are some examples
of nodes:
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

Sometimes you want to track just the presence of the child in the node.

For example this document:
```kdl
plugin "first" {
    auto-start
}
plugin "second"
```
... can be parsed into the list of the following structures:
```rust
#[derive(knuffel::Decode)]
struct Plugin {
    #[knuffel(child)]
    auto_start: bool,
}
```
And in this case `auto-start` node may be omitted without an error even
though it's not wrapped into an `Option`.

No arguments, properties and children are allowed in the boolean nodes.

Note: due to limitations of the procedural macros in Rust, boolean children
must use `bool` in this specific notation. If you shadow `bool` type by some
import the results are undefined (knuffel will still think it's bool node, but
it may not work).


## Unwrapping

The `unwrap` attribute for `child` allows adding extra children in the KDL
document that aren't represented in the final structure, but they play
important role in making document readable.

It works by transforming the following:
```rust,ignore
#[derive(knuffel::Decode)]
struct Node {
    #[knuffel(child, unwrap(/* attributes */))]
    field: String,
}
```
... into something like this:
```
#[derive(knuffel::Decode)]
struct TmpChild {
    #[knuffel(/* attributes */)]
    field: String,
}
#[derive(knuffel::Decode)]
struct Node {
    #[knuffel(child)]
    field: TmpChild,
}
```
... and then unpacks `TmpChild` to put target type into the field.

Most of the attributes can be used in place of `/* attributes */`. Including:
1. `argument` (the most common, see [below](#properties-become-children)) and
   `arguments`
2. `property` (usually in the form of `property(name="other_name")` to
   avoid repetitive KDL) and `properties`
3. `child` and `children` (see example [below](#grouping-things))

Following are some nice examples of using `unwrap`.

### Properties Become Children

In nodes with many properties it might be convenient to put them into children instead.

So instead of this:
```kdl
plugin name="my-plugin" url="https://example.com" {}
```
... users can write this:
```kdl
plugin {
    name "my-plugin"
    url "https://example.com"
}
```

Here is the respective Rust structure:
```rust
#[derive(knuffel::Decode)]
struct Plugin {
    #[knuffel(child, unwrap(argument))]
    name: String,
    #[knuffel(child, unwrap(argument))]
    url: String,
}
```

You can read this like: `name` field parses a child that contains a single
argument of type `String`.

### Grouping Things

Sometimes instead of different kinds of nodes scattered around you may want to
group them.

So instead of this:
```kdl
plugin "a"
file "aa"
plugin "b"
file "bb"
```
You nave a KDL document like this:
```kdl
plugins {
    plugin "a"
    plugin "b"
}
files {
    file "aa"
    file "bb"
}
```
This can be parsed into the following structure:
```rust
# #[derive(knuffel::Decode)] struct Plugin {}
# #[derive(knuffel::Decode)] struct File {}
#[derive(knuffel::Decode)]
struct Document {
    #[knuffel(child, unwrap(children(name="plugin")))]
    plugins: Vec<Plugin>,
    #[knuffel(child, unwrap(children(name="file")))]
    files: Vec<File>,
}
```

You can read this like: `plugins` field parses a child that contains a set of
children named `plugin`.


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

Technically [DecodeChildren](traits/trait.DecodeChildren.html) trait will be
implemented for the structures that can be used as documents.


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

Similarly to `flatten` flag in `serde`, this allows factoring out some
properties or children into another structure.

For example:
```rust
#[derive(knuffel::Decode, Default)]
struct Common {
    #[knuffel(child, unwrap(argument))]
    name: Option<String>,
    #[knuffel(child, unwrap(argument))]
    description: Option<String>
}
#[derive(knuffel::Decode)]
struct Plugin {
    #[knuffel(flatten(child))]
    common: Common,
    #[knuffel(child, unwrap(argument))]
    url: String,
}
```
This will parse the following:
```kdl
plugin {
    name "my-plugin"
    description "Some example plugin"
    url "https://example.org/plugin"
}
```

There are few limitations of the `flatten`:
1. All fields in target structure must be optional.
2. The target structure must implement [`Default`](std::default::Default)
3. Only children an properties can be factored out, not arguments in current
   implementation
4. You must specify which directives can be used in the target structure
    (i.e. `flatten(child, children, property, properties)`) and if `children`
    or `properties` are forwarded to the target structure, no more children
    and property attributes can be used in this structure following the
    `flatten` attribute.

We may lift some of these limitations later.

Technically [DecodePartial](traits/trait.DecodePartial.html) trait will be
implemented for the strucutures that can be used with the `flatten` attribute.


# Special Values

## Type Name

Here is the example of the node with the type name (the name in parens):
```kdl
(text)document name="New Document" { }
```

By default knuffel doesn't allow type names for nodes as these are quite rare.

To allow type names on specific node and to have the name stored use
`type_name` attribute:
```rust
#[derive(knuffel::Decode)]
struct Node {
    #[knuffel(type_name)]
    type_name: String,
}
```
Type name can be optional.

The field that is a target of `type_name` can be any type that implements
`FromStr`. This might be used to validate node type:
```rust
pub enum PluginType {
    Builtin,
    External,
}

impl std::str::FromStr for PluginType {
    type Err = Box<dyn std::error::Error + Send + Sync + 'static>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "builtin" => Ok(PluginType::Builtin),
            "external" => Ok(PluginType::External),
            _ => Err("Plugin type name must be `builtin` or `external`")?,
        }
    }
}

#[derive(knuffel::Decode)]
struct Node {
    #[knuffel(type_name)]
    type_name: PluginType,
}
```

## Node Name

In knuffel, it's common that parent node, document or enum type checks the node name of the node, and node name is not stored or validated in the strucuture.

But for the cases where you need it, it's possible to store too:
```rust
#[derive(knuffel::Decode)]
struct Node {
    #[knuffel(node_name)]
    node_name: String,
}
```

You can use any type that implements `FromStr` to validate node name. Similarly to the example in the [type names](#type-name) section.

Node name always exists so optional node_name is not supported.

## Spans

The following definition:
```rust
use knuffel::span::Span;  // or LineSpan

#[derive(knuffel::Decode)]
#[knuffel(span_type=Span)]
struct Node {
    #[knuffel(span)]
    span: Span,  // This can be user type decoded from Span
}
```
Puts position of the node in the source code into the `span` field. Span
contains the whole node, starting from parenthesis that enclose type name if
present otherwise node name. Includes node children if exists and semicolon or
newline that ends the node (so includes any whitespace and coments before the
newline if node ends by a newline, but doesn't include anything after
semicolon).

The span value might be different than one used for parsing. In this case, it
should implement [`DecodeSpan`](traits/trait.DecodeSpan.html) trait.

Independenly of whether you use custom span type, or built-in one, you have to
specify `span_type` for the decoder, since there is no generic implementation
of the `DecodeSpan` for any type. See [Span Type](#span-type) for more info

# Enums

Enums are used to differentiate nodes by name when multiple kinds of nodes are
pushed to a single collection.

For example, to parse the following list of actions:
```kdl
create "xxx"
print-string "yyy" line=2
finish
```
The following enum might be used:
```rust
# #[derive(knuffel::Decode)] struct PrintString {}
#[derive(knuffel::Decode)]
enum Action {
    Create(#[knuffel(argument)] String),
    PrintString(PrintString),
    Finish,
    #[knuffel(skip)]
    InternalAction,
}
```

The following variants supported:
1. Single element tuple struct without arguments (`PrintString` in example),
   which forwards node parsing to the inner element.
2. Normal `argument`, `arguments`, `properties`, `children` fields (`Create`
   example)
3. Property fields with names `property(name="xxx")`
4. Unit structs, in this case no arguments, properties and children are
   expected in such node
5. Variant with `skip`, cannot be deserialized and can be in any form

Enum variant names are matches against node names converted into `kebab-case`.

# Container Attributes

## Span Type

Usually generated implemenation is for any span type:
```rust,ignore
impl Decode<S> for MyStruct {
   # ...
}
```
But if you want to use `span` argument, it's unlikely to be possible to
implement `DecodeSpan` for any type.

Use use `span_type=` for implemenation of specific type:
```rust
use knuffel::span::Span;  // or LineSpan

#[derive(knuffel::Decode)]
#[knuffel(span_type=Span)]
struct MyStruct {
    #[knuffel(span)]
    span: Span,
}
```
This will generate implementation like this:
```rust,ignore
impl Decode<Span> for MyStruct {
   # ...
}
```

See [Spans](#spans) section for more info about decoding spans.
