use crate::object::Object;

pub struct Builtins;

impl Builtins {
    pub fn all_builtins(&self) -> Vec<(String, Object)> {
        vec![(String::from("len"), Object::Builtin(b_len))]
    }
}

fn b_len(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }
    return match &args[0] {
        Object::StringObj(string_literal) => Object::Integer(string_literal.len() as i64),
        other => Object::Error(format!(
            "argument to 'len' not supported, got {}",
            other.object_type()
        )),
    };
}
