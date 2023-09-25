use crate::{
    ast::{ExpressionNode, Program, StatementNode},
    object::Object,
};

pub struct Evaluator {}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {}
    }

    pub fn eval_program(&self, program: Program) -> Object {
        let mut result = Object::Null;

        for stmt in program.statements {
            result = self.eval_statement(stmt);
        }

        result
    }

    fn eval_statement(&self, stmt: StatementNode) -> Object {
        match stmt {
            StatementNode::Expression(exp_stmt) => self.eval_expression(exp_stmt.expression),
            _ => Object::Null,
        }
    }

    fn eval_expression(&self, expression: Option<ExpressionNode>) -> Object {
        if let Some(exp) = expression {
            return match exp {
                ExpressionNode::Integer(int) => Object::Integer(int.value),
                _ => Object::Null,
            };
        }
        Object::Null
    }
}

#[cfg(test)]
mod test {
    use crate::{lexer::Lexer, object::Object, parser::Parser};

    use super::Evaluator;

    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![("5", 5), ("10", 10)];

        for test in tests {
            let evaluated = test_eval(test.0);
            test_integer_object(evaluated, test.1);
        }
    }

    fn test_eval(input: &str) -> Object {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let evaluator = Evaluator::new();
        evaluator.eval_program(program.unwrap())
    }

    fn test_integer_object(obj: Object, expected: i64) {
        match obj {
            Object::Integer(int) => assert_eq!(
                int, expected,
                "object has wrong value. got={}, want={}",
                int, expected
            ),
            other => panic!("object is not integer. got={:?}", other),
        }
    }
}
