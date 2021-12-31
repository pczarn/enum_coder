## enum_coder

Enum coder macro. A syntancic sugar for generating lists of instructions,
intermediate representations for compilers etc.

Input for the macro consists of one enum definition, followed by function
definitions.

Attributes `generate_list` makes a generator of enum values. The function
signature is `fn f() -> Vec<T>`.

Attribute `missing_field` marks the computation of default values for fields.
Alternatively, the attribute can be written as `missing_field(args, ...)`.
Then, function signature is `fn f(args, field_name: &str)`.

See `examples` for more.

```rust
enum_coder! {
    enum Instruction {
        Push(String),
        Concat(usize),
        ConcatSeparated(usize, String),
        Dup,
        Test,
        ConcatRef {
            ref_a: usize,
            ref_b: usize,
        },
    }

    #[generate_list]
    fn make_instructions() -> Vec<Instruction> {
        Push("hello".to_string());
        Push("world!".to_string());
        // ConcatSeparated(2, ", ".to_string());
        // or
        ConcatRef {
            ref_a: 0,
            ref_b: 1,
        };
        for _ in 0..10 {
            Dup()
        }
        Test();
        Concat(10 + 1)
    }
}
```

Documentation is available here: https://docs.rs/enum_coder/latest/enum_coder/
