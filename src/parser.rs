use std::collections::HashMap;

use crate::{
    ast::{
        Boolean, ExpressionNode, ExpressionStatement, Identifier, InfixExpression, IntegerLiteral,
        LetStatement, PrefixExpression, Program, ReturnStatement, StatementNode,
    },
    lexer::Lexer,
    token::{Token, TokenKind},
};

type PrefixParseFn = fn(parser: &mut Parser) -> Option<ExpressionNode>;
type InfixParseFn = fn(parser: &mut Parser, exp: ExpressionNode) -> Option<ExpressionNode>;

#[derive(Debug, Copy, Clone)]
enum PrecedenceLevel {
    Lowest = 0,
    Equals = 1,      // ==
    LessGreater = 2, // > or <
    Sum = 3,         // +
    Product = 4,
    Prefix = 5,
    Call = 6,
}

fn precedence_map(kind: &TokenKind) -> PrecedenceLevel {
    return match kind {
        TokenKind::Eq => PrecedenceLevel::Equals,
        TokenKind::NotEq => PrecedenceLevel::Equals,
        TokenKind::Lt => PrecedenceLevel::LessGreater,
        TokenKind::Gt => PrecedenceLevel::LessGreater,
        TokenKind::Plus => PrecedenceLevel::Sum,
        TokenKind::Minus => PrecedenceLevel::Sum,
        TokenKind::Slash => PrecedenceLevel::Product,
        TokenKind::Asterisk => PrecedenceLevel::Product,
        _ => PrecedenceLevel::Lowest,
    };
}

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
    prefix_parse_fns: HashMap<TokenKind, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenKind, InfixParseFn>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        let mut parser = Parser {
            lexer,
            cur_token: Default::default(),
            peek_token: Default::default(),
            errors: vec![],
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        parser.register_prefix(TokenKind::Ident, Self::parse_identifier);
        parser.register_prefix(TokenKind::Int, Self::parse_integer_literal);
        parser.register_prefix(TokenKind::Bang, Self::parse_prefix_expression);
        parser.register_prefix(TokenKind::Minus, Self::parse_prefix_expression);
        parser.register_prefix(TokenKind::True, Self::parse_boolean);
        parser.register_prefix(TokenKind::False, Self::parse_boolean);

        parser.register_infix(TokenKind::Plus, Self::parse_infix_expression);
        parser.register_infix(TokenKind::Minus, Self::parse_infix_expression);
        parser.register_infix(TokenKind::Slash, Self::parse_infix_expression);
        parser.register_infix(TokenKind::Asterisk, Self::parse_infix_expression);
        parser.register_infix(TokenKind::Eq, Self::parse_infix_expression);
        parser.register_infix(TokenKind::NotEq, Self::parse_infix_expression);
        parser.register_infix(TokenKind::Lt, Self::parse_infix_expression);
        parser.register_infix(TokenKind::Gt, Self::parse_infix_expression);

        parser.next_token();
        parser.next_token();

        parser
    }

    fn parse_identifier(&mut self) -> Option<ExpressionNode> {
        Some(ExpressionNode::IdentifierNode(Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        }))
    }

    fn parse_integer_literal(&mut self) -> Option<ExpressionNode> {
        let mut literal = IntegerLiteral {
            token: self.cur_token.clone(),
            value: Default::default(),
        };

        return match self.cur_token.literal.parse::<i64>() {
            Ok(value) => {
                literal.value = value;
                Some(ExpressionNode::Integer(literal))
            }
            Err(_) => {
                let msg = format!("could not parse {} as integer", self.cur_token.literal);
                self.errors.push(msg);
                None
            }
        };
    }

    fn parse_prefix_expression(&mut self) -> Option<ExpressionNode> {
        let mut expression = PrefixExpression {
            token: self.cur_token.clone(),
            operator: self.cur_token.literal.clone(),
            right: Default::default(),
        };
        self.next_token();
        match self.parse_expression(PrecedenceLevel::Prefix) {
            Some(exp) => expression.right = Box::new(exp),
            None => return None,
        }

        Some(ExpressionNode::Prefix(expression))
    }

    fn parse_boolean(&mut self) -> Option<ExpressionNode> {
        Some(ExpressionNode::BooleanNode(Boolean {
            token: self.cur_token.clone(),
            value: self.cur_token_is(TokenKind::True),
        }))
    }

    fn parse_infix_expression(&mut self, left: ExpressionNode) -> Option<ExpressionNode> {
        self.next_token();
        let mut expression = InfixExpression {
            token: self.cur_token.clone(),
            operator: self.cur_token.literal.clone(),
            left: Box::new(left),
            right: Default::default(),
        };
        let precedence = self.cur_precedence();
        self.next_token();
        match self.parse_expression(precedence) {
            Some(exp) => expression.right = Box::new(exp),
            None => return None,
        }

        Some(ExpressionNode::Infix(expression))
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Option<Program> {
        let mut program = Program { statements: vec![] };

        while !self.cur_token_is(TokenKind::Eof) {
            if let Some(statement) = self.parse_statement() {
                program.statements.push(statement);
            }
            self.next_token();
        }

        Some(program)
    }

    fn parse_statement(&mut self) -> Option<StatementNode> {
        match self.cur_token.kind {
            TokenKind::Let => self.parse_let_statement(),
            TokenKind::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> Option<StatementNode> {
        let stmt = ExpressionStatement {
            token: self.cur_token.clone(),
            expression: self.parse_expression(PrecedenceLevel::Lowest),
        };

        // 5 + 5, 5 + 5;
        if self.peek_token_is(TokenKind::Semicolon) {
            self.next_token();
        }

        Some(StatementNode::Expression(stmt))
    }

    fn parse_expression(&mut self, precedence_level: PrecedenceLevel) -> Option<ExpressionNode> {
        let prefix = self.prefix_parse_fns.get(&self.cur_token.kind);
        if let Some(prefix_fn) = prefix {
            let mut left_exp = prefix_fn(self);

            while !self.peek_token_is(TokenKind::Semicolon)
                && (precedence_level as u8) < (self.peek_precedence() as u8)
            {
                let infix_fn = self.infix_parse_fns.get(&self.peek_token.kind);
                if let Some(infix) = infix_fn {
                    left_exp = infix(self, left_exp.expect("left exp should be present"))
                }
            }
            return left_exp;
        }
        self.no_prefix_parse_fn_error(self.cur_token.kind.clone());
        None
    }

    fn no_prefix_parse_fn_error(&mut self, token_kind: TokenKind) {
        let msg = format!("no prefix parse function for {} found", token_kind);
        self.errors.push(msg)
    }

    fn parse_let_statement(&mut self) -> Option<StatementNode> {
        let mut stmt = LetStatement {
            token: self.cur_token.clone(),
            name: Default::default(),
            value: Default::default(),
        };

        return if !self.expect_peek(TokenKind::Ident) {
            None
        } else {
            stmt.name = Identifier {
                token: self.cur_token.clone(),
                value: self.cur_token.literal.clone(),
            };

            if !self.expect_peek(TokenKind::Assign) {
                None
            } else {
                self.next_token();
                // TODO: need to parse expression
                while !self.cur_token_is(TokenKind::Semicolon) {
                    self.next_token();
                }
                Some(StatementNode::Let(stmt))
            }
        };
    }

    fn parse_return_statement(&mut self) -> Option<StatementNode> {
        let stmt = ReturnStatement {
            token: self.cur_token.clone(),
            ret_value: Default::default(),
        };
        self.next_token();

        while !self.cur_token_is(TokenKind::Semicolon) {
            self.next_token();
        }

        Some(StatementNode::Return(stmt))
    }

    fn expect_peek(&mut self, token_kind: TokenKind) -> bool {
        if self.peek_token_is(token_kind.clone()) {
            self.next_token();
            return true;
        }
        self.peek_error(token_kind);
        false
    }

    fn peek_token_is(&self, token_kind: TokenKind) -> bool {
        self.peek_token.kind == token_kind
    }

    fn cur_token_is(&self, token_kind: TokenKind) -> bool {
        self.cur_token.kind == token_kind
    }

    fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    fn peek_error(&mut self, token_kind: TokenKind) {
        let msg = format!(
            "expected next token to be {}, got {} instead",
            token_kind, self.peek_token.kind
        );
        self.errors.push(msg);
    }

    fn register_prefix(&mut self, token_kind: TokenKind, prefix_fn: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_kind, prefix_fn);
    }

    fn register_infix(&mut self, token_kind: TokenKind, infix_fn: InfixParseFn) {
        self.infix_parse_fns.insert(token_kind, infix_fn);
    }

    fn peek_precedence(&self) -> PrecedenceLevel {
        precedence_map(&self.peek_token.kind)
    }

    fn cur_precedence(&self) -> PrecedenceLevel {
        precedence_map(&self.cur_token.kind)
    }
}

#[cfg(test)]
mod test {
    use std::any;

    use crate::{
        ast::{ExpressionNode, Identifier, Node, StatementNode},
        lexer::Lexer,
        token::TokenKind,
    };

    use super::Parser;

    #[test]
    fn test_let_statements() {
        let input = r#"
        let x = 5;
        let y = 10;
        let foobar = 838383;
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(parser);

        match program {
            Some(program) => {
                assert_eq!(
                    program.statements.len(),
                    3,
                    "statements does not contain 3 statements. got={}",
                    program.statements.len()
                );

                let expected = vec!["x", "y", "foobar"];

                for (idx, exp) in expected.into_iter().enumerate() {
                    let stmt = &program.statements[idx];
                    test_let_statement(stmt, exp);
                }
            }
            None => panic!("parse program should not be none"),
        }
    }

    #[test]
    fn test_return_statements() {
        let input = r#"
            return 5;
            return 10;
            return 993322;
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(parser);

        match program {
            Some(program) => {
                assert_eq!(
                    program.statements.len(),
                    3,
                    "statements does not contain 3 statements. got={}",
                    program.statements.len()
                );

                for stmt in program.statements {
                    match stmt {
                        StatementNode::Return(ret_stmt) => {
                            assert_eq!(
                                ret_stmt.token_literal(),
                                "return",
                                "token literal not `return`. got={}",
                                ret_stmt.token_literal()
                            );
                        }
                        other => panic!("not ReturnStatement. got={:?}", other),
                    }
                }
            }
            None => panic!("parse program should not be none"),
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap();
        check_parser_errors(parser);

        assert_eq!(
            program.statements.len(),
            1,
            "program.statements does not contain enough statements. got={}",
            program.statements.len()
        );

        match &program.statements[0] {
            StatementNode::Expression(exp_stmt) => {
                assert!(exp_stmt.expression.is_some());

                match exp_stmt.expression.as_ref().unwrap() {
                    ExpressionNode::IdentifierNode(identifier) => {
                        assert_eq!(
                            identifier.value, "foobar",
                            "identifier value not `foobar`. got={}",
                            identifier.value
                        );
                        assert_eq!(
                            identifier.token_literal(),
                            "foobar",
                            "identifier.token_literal() is not `foobar`. got={}",
                            identifier.token_literal()
                        );
                    }
                    other => panic!("expression not identifier. got={:?}", other),
                }
            }
            other => panic!(
                "program.statements[0] is not ExpressionStatement. got={:?}",
                other
            ),
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap();
        check_parser_errors(parser);

        assert_eq!(
            program.statements.len(),
            1,
            "program.statements does not contain enough statements. got={}",
            program.statements.len()
        );

        match &program.statements[0] {
            StatementNode::Expression(exp_stmt) => {
                assert!(exp_stmt.expression.is_some());
                match exp_stmt.expression.as_ref().unwrap() {
                    ExpressionNode::Integer(integer) => {
                        assert_eq!(
                            integer.value, 5,
                            "integer.value not `5`. got={}",
                            integer.value
                        );
                        assert_eq!(
                            integer.token_literal(),
                            "5",
                            "integer.value not `5`. got={}",
                            integer.token_literal()
                        )
                    }
                    other => panic!("Expression not an IntegerLiteral. got={:?}", other),
                }
            }
            other => panic!(
                "program.statements[0] is not ExpressionStatement. got={:?}",
                other
            ),
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let prefix_tests: Vec<(&str, &str, Box<dyn any::Any>)> = vec![
            ("!5", "!", Box::new(5)),
            ("-15", "-", Box::new(15)),
            ("!true", "!", Box::new(true)),
            ("!false", "!", Box::new(false)),
        ];

        for test in prefix_tests {
            let lexer = Lexer::new(test.0);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program().unwrap();
            check_parser_errors(parser);

            assert_eq!(
                program.statements.len(),
                1,
                "program.statements does not contain enough statements. got={}",
                program.statements.len()
            );

            match &program.statements[0] {
                StatementNode::Expression(exp_stmt) => {
                    assert!(exp_stmt.expression.is_some());
                    let exp = exp_stmt.expression.as_ref().unwrap();

                    match exp {
                        ExpressionNode::Prefix(prefix_exp) => {
                            assert_eq!(
                                prefix_exp.operator, test.1,
                                "prefix_exp.operator not {}. got={}",
                                test.1, prefix_exp.operator
                            );
                            test_literal_expression(&prefix_exp.right, test.2);
                        }
                        other => panic!("exp not PrefixExpression. got={:?}", other),
                    }
                }
                other => panic!(
                    "program.statements[0] is not ExpressionStatement. got={:?}",
                    other
                ),
            }
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let infix_tests: Vec<(&str, Box<dyn any::Any>, &str, Box<dyn any::Any>)> = vec![
            ("5 + 5;", Box::new(5), "+", Box::new(5)),
            ("5 - 5;", Box::new(5), "-", Box::new(5)),
            ("5 * 5;", Box::new(5), "*", Box::new(5)),
            ("5 / 5;", Box::new(5), "/", Box::new(5)),
            ("5 > 5;", Box::new(5), ">", Box::new(5)),
            ("5 < 5;", Box::new(5), "<", Box::new(5)),
            ("5 == 5;", Box::new(5), "==", Box::new(5)),
            ("5 != 5;", Box::new(5), "!=", Box::new(5)),
            ("true == true", Box::new(true), "==", Box::new(true)),
            ("true != false", Box::new(true), "!=", Box::new(false)),
            ("false == false", Box::new(false), "==", Box::new(false)),
        ];

        for test in infix_tests {
            let lexer = Lexer::new(test.0);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program().unwrap();
            check_parser_errors(parser);

            assert_eq!(
                program.statements.len(),
                1,
                "program.statements does not contain enough statements. got={}",
                program.statements.len()
            );

            match &program.statements[0] {
                StatementNode::Expression(exp_stmt) => {
                    assert!(exp_stmt.expression.is_some());
                    let exp = exp_stmt.expression.as_ref().unwrap();

                    test_infix_expression(
                        exp,
                        Box::new(test.1),
                        test.2.to_string(),
                        Box::new(test.3),
                    )
                }
                other => panic!(
                    "program.statements[0] is not ExpressionStatement. got={:?}",
                    other
                ),
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
        ];

        for test in tests {
            let lexer = Lexer::new(test.0);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program().unwrap();
            check_parser_errors(parser);

            let actual = program.print_string();
            assert_eq!(actual, test.1, "expected={}, got={}", test.1, actual);
        }
    }

    #[test]
    fn test_boolean_expression() {
        let input = r#"
            true;
            false;
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap();
        check_parser_errors(parser);

        assert_eq!(
            program.statements.len(),
            2,
            "program.statements does not contain enough statements. got={}",
            program.statements.len()
        );

        let expected_values = vec![(TokenKind::True, "true"), (TokenKind::False, "false")];

        for (idx, test) in expected_values.into_iter().enumerate() {
            match &program.statements[idx] {
                StatementNode::Expression(exp_stmt) => {
                    assert!(exp_stmt.expression.is_some());
                    let exp = exp_stmt.expression.as_ref().unwrap();

                    match exp {
                        ExpressionNode::BooleanNode(bool_exp) => {
                            assert_eq!(
                                bool_exp.token.kind, test.0,
                                "token kind is not expected. got={}",
                                bool_exp.token.kind
                            );
                            assert_eq!(
                                bool_exp.token_literal(),
                                test.1,
                                "token literal does not match. got={}",
                                bool_exp.token_literal()
                            );
                        }
                        other => panic!("expression is not Boolean, got={:?}", other),
                    }
                }
                other => panic!(
                    "program.statements[{}] is not ExpressionStatement, got={:?}",
                    idx, other
                ),
            }
        }
    }

    fn test_let_statement(stmt: &StatementNode, expected: &str) {
        assert_eq!(
            stmt.token_literal(),
            "let",
            "token literal not `let`. got={}",
            stmt.token_literal()
        );
        match stmt {
            StatementNode::Let(let_stmt) => {
                assert_eq!(
                    let_stmt.name.value, expected,
                    "LetStatement name value not {}. got {}",
                    expected, let_stmt.name.value
                );
                assert_eq!(
                    let_stmt.name.token_literal(),
                    expected,
                    "LetStatement name value not {}. got {}",
                    expected,
                    let_stmt.name.token_literal()
                );
            }
            other => panic!("not a Let Statement. got={:?}", other),
        }
    }

    fn check_parser_errors(parser: Parser) {
        let errors = parser.errors();

        if errors.len() == 0 {
            return;
        }

        for error in errors {
            eprintln!("parser error: {}", error);
        }

        panic!("parser error present");
    }

    fn test_integer_literal(exp: &ExpressionNode, value: i64) {
        match exp {
            ExpressionNode::Integer(int_exp) => {
                assert_eq!(
                    int_exp.value, value,
                    "int_exp.value not {}, got={}",
                    value, int_exp.value
                );
                assert_eq!(
                    int_exp.token_literal(),
                    format!("{}", value),
                    "int_exp.token_literal() not {}, got={}",
                    value,
                    int_exp.token_literal()
                );
            }
            other => panic!("exp not IntegerLiteral. got={:?}", other),
        }
    }

    fn test_identifier(exp: &ExpressionNode, value: String) {
        match exp {
            ExpressionNode::IdentifierNode(identifier_exp) => {
                assert_eq!(
                    identifier_exp.value, value,
                    "identifier_exp.value not {}, got={}",
                    value, identifier_exp.value
                );
                assert_eq!(
                    identifier_exp.token_literal(),
                    value,
                    "identifier_exp.token_literal not {}, got={}",
                    value,
                    identifier_exp.token_literal()
                );
            }
            other => panic!("exp not Identifier. got={:?}", other),
        }
    }

    fn test_literal_expression(exp: &ExpressionNode, expected: Box<dyn any::Any>) {
        match expected.downcast_ref::<String>() {
            Some(exp_string) => test_identifier(exp, exp_string.to_string()),
            None => match expected.downcast_ref::<i64>() {
                Some(int_exp) => test_integer_literal(exp, int_exp.to_owned()),
                None => match expected.downcast_ref::<bool>() {
                    Some(bool) => test_boolean_literal(exp, bool.to_owned()),
                    None => (),
                },
            },
        }
    }

    fn test_boolean_literal(exp: &ExpressionNode, value: bool) {
        match exp {
            ExpressionNode::BooleanNode(bool_exp) => {
                assert_eq!(
                    bool_exp.value, value,
                    "bool_exp.value not {}, got={}",
                    value, bool_exp.value
                );
                assert_eq!(
                    bool_exp.token_literal(),
                    format!("{}", value),
                    "bool_exp.token_literal not {}, got={}",
                    value,
                    bool_exp.token_literal()
                );
            }
            other => panic!("exp is not Boolean. got={:?}", other),
        }
    }

    fn test_infix_expression(
        exp: &ExpressionNode,
        left: Box<dyn any::Any>,
        operator: String,
        right: Box<dyn any::Any>,
    ) {
        match exp {
            ExpressionNode::Infix(infix_exp) => {
                test_literal_expression(&infix_exp.left, left);
                assert_eq!(
                    infix_exp.operator, operator,
                    "operator is not {}. got={}",
                    operator, infix_exp.operator
                );
                test_literal_expression(&infix_exp.right, right);
            }
            other => panic!("exp is not InfixExpression. got={:?}", other),
        }
    }
}
