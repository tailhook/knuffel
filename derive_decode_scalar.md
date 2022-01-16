Currently `DecodeScalar` derive is only implemented for enums

# Enums

Only enums that contain no data are supported:
```rust
#[derive(knuffel::DecodeScalar)]
enum Color {
    Red,
    Blue,
    Green,
    InfraRed,
}
```

This will match scalar values in `kebab-case`. For example, this node decoder:
```
# #[derive(knuffel::DecodeScalar)]
# enum Color { Red, Blue, Green, InfraRed }
#[derive(knuffel::Decode)]
struct Document {
    #[knuffel(child, unwrap(arguments))]
    all_colors: Vec<Color>,
}
```

Can be populated from the following text:
```kdl
all-colors "red" "blue" "green" "infra-red"
```
