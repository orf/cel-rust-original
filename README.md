# Common Expression Language (Rust)

The Common Expression Language (CEL) is a non-Turing complete language designed for simplicity, speed, safety, and
portability. CEL's C-like syntax looks nearly identical to equivalent expressions in C++, Go, Java, and TypeScript. CEL
is ideal for lightweight expression evaluation when a fully sandboxed scripting language is too resource intensive.

```java
// Check whether a resource name starts with a group name.
resource.name.startsWith("/groups/" + auth.claims.group)
```

```go
// Determine whether the request is in the permitted time window.
request.time - resource.age < duration("24h")
```

```typescript
// Check whether all resource names in a list match a given filter.
auth.claims.email_verified && resources.all(r, r.startsWith(auth.claims.email))
```

## Getting Started
This project includes a CEL-parser and an interpreter which means that it can be used to evaluate CEL-expressions. The library aims to be very simple to use, while still being fast, safe, and customizable.

```rust
fn main() {
    let program = Program::compile("1 == 1").unwrap();
    let context = Context::default();
    let value = program.execute(&context).unwrap();
    assert_eq!(value, true.into());
}
```