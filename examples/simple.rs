extern crate enum_coder;

use enum_coder::enum_coder;

enum_coder! {
    enum Instruction {
        Push(String),
        Concat(usize),
        ConcatSeparated(usize, String),
        Dup,
        Test {
            field: String,
        }
    }

    #[missing_field]
    fn missing(field_name: &str) -> String {
        format!("{}...", field_name)
    }

    #[generate_list]
    fn make_instructions() -> Vec<Instruction> {
        Push("hello".to_string());
        Push("world!".to_string());
        ConcatSeparated(2, ", ".to_string());
        for _ in 0..10 {
            Dup()
        }
        Test();
        Test {};
        Test {
            field: "hello, multiverse!".to_string()
        };
        Concat(10 + 3)
    }
}

struct Stack {
    v: Vec<String>
}

impl Stack {
    fn pop(&mut self) -> String {
        self.v.pop().expect("stack error")
    }

    fn run(&mut self, ins: Instruction) {
        match ins {
            Instruction::Push(s) => self.v.push(s),
            Instruction::Concat(n) => {
                let v = self.v.split_off(self.v.len().checked_sub(n).expect("stack error"));
                self.v.push(v[..].concat());
            }
            Instruction::ConcatSeparated(n, sep) => {
                let v = self.v.split_off(self.v.len().checked_sub(n).expect("stack error"));
                self.v.push(v[..].join(&sep[..]));
            }
            Instruction::Dup => {
                let elem = self.pop();
                self.v.push(elem.clone());
                self.v.push(elem);
            }
            Instruction::Test { field } => self.v.push(field),
        }
    }
}

#[test]
fn test_instructions() {
    let insns = make_instructions();
    let mut state = Stack { v: vec![] };
    for ins in insns {
        state.run(ins);
    }
    assert_eq!(
        state.pop(),
        "hello, world!\
        hello, world!\
        hello, world!\
        hello, world!\
        hello, world!\
        hello, world!\
        hello, world!\
        hello, world!\
        hello, world!\
        hello, world!\
        field...\
        field...\
        hello, multiverse!"
    )
}
