use std::collections::HashMap;

use crate::{
    ast::{
        ArrayLiteral, BlockStatement, Boolean, CallExpression, ExpressionNode, ExpressionStatement,
        FunctionLiteral, Identifier, IfExpression, IndexExpression, InfixExpression,
        IntegerLiteral, LetStatement, PrefixExpression, Program, ReturnStatement, StatementNode,
        StringLiteral,
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
    Index = 7,
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
        TokenKind::Lparen => PrecedenceLevel::Call,
        TokenKind::Lbracket => PrecedenceLevel::Index,
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
        parser.register_prefix(TokenKind::Lparen, Self::parse_grouped_expression);
        parser.register_prefix(TokenKind::If, Self::parse_if_expression);
        parser.register_prefix(TokenKind::Function, Self::parse_function_literal);
        parser.register_prefix(TokenKind::String, Self::parse_string_literal);
        parser.register_prefix(TokenKind::Lbracket, Self::parse_array_literal);

        parser.register_infix(TokenKind::Plus, Self::parse_infix_expression);
        parser.register_infix(TokenKind::Minus, Self::parse_infix_expression);
        parser.register_infix(TokenKind::Slash, Self::parse_infix_expression);
        parser.register_infix(TokenKind::Asterisk, Self::parse_infix_expression);
        parser.register_infix(TokenKind::Eq, Self::parse_infix_expression);
        parser.register_infix(TokenKind::NotEq, Self::parse_infix_expression);
        parser.register_infix(TokenKind::Lt, Self::parse_infix_expression);
        parser.register_infix(TokenKind::Gt, Self::parse_infix_expression);
        parser.register_infix(TokenKind::Lparen, Self::parse_call_expression);
        parser.register_infix(TokenKind::Lbracket, Self::parse_index_expression);

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

    fn parse_grouped_expression(&mut self) -> Option<ExpressionNode> {
        self.next_token();

        let exp = self.parse_expression(PrecedenceLevel::Lowest);

        if !self.expect_peek(TokenKind::Rparen) {
            return None;
        }

        exp
    }

    fn parse_if_expression(&mut self) -> Option<ExpressionNode> {
        let mut expression = IfExpression {
            token: self.cur_token.clone(),
            alternative: None,
            condition: Default::default(),
            consequence: Default::default(),
        };

        if !self.expect_peek(TokenKind::Lparen) {
            return None;
        }

        self.next_token();
        expression.condition = Box::new(
            self.parse_expression(PrecedenceLevel::Lowest)
                .expect("error parsing condition"),
        );

        if !self.expect_peek(TokenKind::Rparen) {
            return None;
        }

        if !self.expect_peek(TokenKind::Lbrace) {
            return None;
        }

        expression.consequence = self.parse_block_statement();

        if self.peek_token_is(TokenKind::Else) {
            self.next_token();

            if !self.expect_peek(TokenKind::Lbrace) {
                return None;
            }

            expression.alternative = Some(self.parse_block_statement());
        }

        Some(ExpressionNode::IfExpressionNode(expression))
    }

    fn parse_function_literal(&mut self) -> Option<ExpressionNode> {
        let mut literal = FunctionLiteral {
            token: self.cur_token.clone(),
            body: Default::default(),
            parameters: vec![],
        };

        if !self.expect_peek(TokenKind::Lparen) {
            return None;
        }

        literal.parameters = self
            .parse_function_parameters()
            .expect("error parsing parameters");

        if !self.expect_peek(TokenKind::Lbrace) {
            return None;
        }

        literal.body = self.parse_block_statement();

        Some(ExpressionNode::Function(literal))
    }

    fn parse_string_literal(&mut self) -> Option<ExpressionNode> {
        Some(ExpressionNode::StringExp(StringLiteral {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        }))
    }

    fn parse_array_literal(&mut self) -> Option<ExpressionNode> {
        let array = ArrayLiteral {
            token: self.cur_token.clone(),
            elements: self.parse_expression_list(TokenKind::Rbracket),
        };
        Some(ExpressionNode::Array(array))
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Identifier>> {
        let mut identifiers = vec![];

        if self.peek_token_is(TokenKind::Rparen) {
            self.next_token();
            return Some(identifiers);
        }

        self.next_token();

        let ident = Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };

        identifiers.push(ident);

        while self.peek_token_is(TokenKind::Comma) {
            self.next_token();
            self.next_token();
            let ident = Identifier {
                token: self.cur_token.clone(),
                value: self.cur_token.literal.clone(),
            };
            identifiers.push(ident);
        }

        if !self.expect_peek(TokenKind::Rparen) {
            return None;
        }

        Some(identifiers)
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let mut block = BlockStatement {
            token: self.cur_token.clone(),
            statements: vec![],
        };

        self.next_token();

        while !self.cur_token_is(TokenKind::Rbrace) && !self.cur_token_is(TokenKind::Eof) {
            let stmt = self.parse_statement();

            if let Some(stmt) = stmt {
                block.statements.push(stmt);
            }
            self.next_token();
        }

        block
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

    fn parse_call_expression(&mut self, function: ExpressionNode) -> Option<ExpressionNode> {
        self.next_token();
        let mut exp = CallExpression {
            token: self.cur_token.clone(),
            function: Box::new(function),
            arguments: vec![],
        };

        exp.arguments = self.parse_expression_list(TokenKind::Rparen);

        Some(ExpressionNode::Call(exp))
    }

    fn parse_index_expression(&mut self, left: ExpressionNode) -> Option<ExpressionNode> {
        self.next_token();
        let mut exp = IndexExpression {
            token: self.cur_token.clone(),
            left: Box::new(left),
            index: Default::default(),
        };

        self.next_token();

        exp.index = Box::new(
            self.parse_expression(PrecedenceLevel::Lowest)
                .expect("error parsing index expression"),
        );

        if !self.expect_peek(TokenKind::Rbracket) {
            return None;
        }

        Some(ExpressionNode::Index(exp))
    }

    fn parse_expression_list(&mut self, end: TokenKind) -> Vec<ExpressionNode> {
        let mut args = vec![];

        if self.peek_token_is(end.clone()) {
            self.next_token();
            return args;
        }

        self.next_token();
        args.push(
            self.parse_expression(PrecedenceLevel::Lowest)
                .expect("error parsing arguments"),
        );

        while self.peek_token_is(TokenKind::Comma) {
            self.next_token();
            self.next_token();
            args.push(
                self.parse_expression(PrecedenceLevel::Lowest)
                    .expect("error parsing arguments"),
            )
        }

        if !self.expect_peek(end) {
            return vec![];
        }

        args
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
                stmt.value = self.parse_expression(PrecedenceLevel::Lowest);
                if self.peek_token_is(TokenKind::Semicolon) {
                    self.next_token();
                }
                Some(StatementNode::Let(stmt))
            }
        };
    }

    fn parse_return_statement(&mut self) -> Option<StatementNode> {
        let mut stmt = ReturnStatement {
            token: self.cur_token.clone(),
            ret_value: Default::default(),
        };
        self.next_token();

        stmt.ret_value = self.parse_expression(PrecedenceLevel::Lowest);

        if self.peek_token_is(TokenKind::Semicolon) {
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

    pub fn errors(&self) -> &Vec<String> {
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
        let tests: Vec<(&str, &str, Box<dyn any::Any>)> = vec![
            ("let x = 5;", "x", Box::new(5)),
            ("let y = true;", "y", Box::new(true)),
            ("let foobar = y;", "foobar", Box::new("y")),
        ];

        for test in tests {
            let lexer = Lexer::new(test.0);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(parser);

            let program = program.unwrap();
            assert_eq!(
                program.statements.len(),
                1,
                "statements does not contain 1 statements. got={}",
                program.statements.len()
            );

            let stmt = &program.statements[0];
            test_let_statement(stmt, test.1);

            match stmt {
                StatementNode::Let(let_stmt) => test_literal_expression(
                    let_stmt
                        .value
                        .as_ref()
                        .expect("error parsing value of let statement"),
                    test.2,
                ),
                other => panic!("expected LetStatement. got={:?}", other),
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let tests: Vec<(&str, Box<dyn any::Any>)> = vec![
            ("return 5;", Box::new(5)),
            ("return 10;", Box::new(10)),
            ("return 993322;", Box::new(993322)),
        ];

        for test in tests {
            let lexer = Lexer::new(test.0);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(parser);

            let program = program.unwrap();
            assert_eq!(
                program.statements.len(),
                1,
                "statements does not contain 1 statements. got={}",
                program.statements.len()
            );

            let stmt = &program.statements[0];

            match stmt {
                StatementNode::Return(ret_stmt) => {
                    assert_eq!(
                        ret_stmt.token_literal(),
                        "return",
                        "token literal not `return`, got={}",
                        ret_stmt.token_literal()
                    );
                    test_literal_expression(
                        ret_stmt
                            .ret_value
                            .as_ref()
                            .expect("error parsing value of let statement"),
                        test.1,
                    );
                }
                other => panic!("expected ReturnStatement. got={:?}", other),
            }
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
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
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

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap();
        check_parser_errors(parser);

        assert_eq!(
            program.statements.len(),
            1,
            "statements does not contain {} statements. got={}",
            1,
            program.statements.len()
        );

        match &program.statements[0] {
            StatementNode::Expression(exp_stmt) => match exp_stmt.expression.as_ref().unwrap() {
                ExpressionNode::IfExpressionNode(if_exp) => {
                    test_infix_expression(
                        &if_exp.condition,
                        Box::new("x"),
                        String::from("<"),
                        Box::new("y"),
                    );
                    assert_eq!(
                        if_exp.consequence.statements.len(),
                        1,
                        "consequence is not 1 statement. got={}",
                        if_exp.consequence.statements.len()
                    );

                    match &if_exp.consequence.statements[0] {
                        StatementNode::Expression(consequence) => test_identifier(
                            consequence
                                .expression
                                .as_ref()
                                .expect("error parsing consequence"),
                            String::from("x"),
                        ),
                        other => panic!("statement is not ExpressionStatement. got={:?}", other),
                    }

                    assert!(if_exp.alternative.is_none());
                }
                other => panic!("Expression is not IfExpression. got={:?}", other),
            },
            other => panic!("statement is not an ExpressionStatement. got={:?}", other),
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap();
        check_parser_errors(parser);

        assert_eq!(
            program.statements.len(),
            1,
            "statements does not contain {} statements. got={}",
            1,
            program.statements.len()
        );

        match &program.statements[0] {
            StatementNode::Expression(exp_stmt) => match exp_stmt.expression.as_ref().unwrap() {
                ExpressionNode::IfExpressionNode(if_exp) => {
                    test_infix_expression(
                        &if_exp.condition,
                        Box::new("x"),
                        String::from("<"),
                        Box::new("y"),
                    );
                    assert_eq!(
                        if_exp.consequence.statements.len(),
                        1,
                        "consequence is not 1 statement. got={}",
                        if_exp.consequence.statements.len()
                    );

                    assert_eq!(
                        if_exp.alternative.as_ref().unwrap().statements.len(),
                        1,
                        "consequence is not 1 statement. got={}",
                        if_exp.alternative.as_ref().unwrap().statements.len()
                    );

                    match &if_exp.consequence.statements[0] {
                        StatementNode::Expression(consequence) => test_identifier(
                            consequence
                                .expression
                                .as_ref()
                                .expect("error parsing consequence"),
                            String::from("x"),
                        ),
                        other => panic!("statement is not ExpressionStatement. got={:?}", other),
                    }

                    match &if_exp.alternative.as_ref().unwrap().statements[0] {
                        StatementNode::Expression(alternative) => test_identifier(
                            alternative
                                .expression
                                .as_ref()
                                .expect("error parsing alternative"),
                            String::from("y"),
                        ),
                        other => panic!("statement is not ExpressionStatement. got={:?}", other),
                    }
                }
                other => panic!("Expression is not IfExpression. got={:?}", other),
            },
            other => panic!("statement is not an ExpressionStatement. got={:?}", other),
        }
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y; }";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap();
        check_parser_errors(parser);

        assert_eq!(
            program.statements.len(),
            1,
            "statements does not contain 1 statements, got={}",
            program.statements.len()
        );

        match &program.statements[0] {
            StatementNode::Expression(exp_stmt) => match exp_stmt.expression.as_ref().unwrap() {
                ExpressionNode::Function(fn_lit) => {
                    assert_eq!(
                        fn_lit.parameters.len(),
                        2,
                        "function literal parameters wrong. Expected 2 got={}",
                        fn_lit.parameters.len()
                    );

                    match &fn_lit.parameters[0] {
                        Identifier { token, value } => {
                            assert_eq!(value, "x", "parameter wrong. Expected `x` got={}", value);
                            assert_eq!(
                                token.literal, "x",
                                "parameter wrong. Expected `x` got={}",
                                token.literal
                            );
                        }
                    }

                    match &fn_lit.parameters[1] {
                        Identifier { token, value } => {
                            assert_eq!(value, "y", "parameter wrong. Expected `y` got={}", value);
                            assert_eq!(
                                token.literal, "y",
                                "parameter wrong. Expected `y` got={}",
                                token.literal
                            );
                        }
                    }

                    assert_eq!(
                        fn_lit.body.statements.len(),
                        1,
                        "function body statements wrong. Expected 2 got={}",
                        fn_lit.body.statements.len()
                    );

                    match &fn_lit.body.statements[0] {
                        StatementNode::Expression(exp) => test_infix_expression(
                            exp.expression.as_ref().unwrap(),
                            Box::new("x"),
                            String::from("+"),
                            Box::new("y"),
                        ),
                        other => panic!(
                            "function body statement is not ExpressionStatement. got={:?}",
                            other
                        ),
                    }
                }
                other => panic!("Expression is not FunctionLiteral. got={:?}", other),
            },
            other => panic!("statement is not an ExpressionStatement. got={:?}", other),
        }
    }

    #[test]
    fn test_function_parameter_parsing() {
        let tests = vec![
            ("fn() {};", vec![]),
            ("fn(x) {};", vec!["x"]),
            ("fn(x, y, z) {};", vec!["x", "y", "z"]),
        ];

        for test in tests {
            let lexer = Lexer::new(test.0);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program().unwrap();
            check_parser_errors(parser);

            match &program.statements[0] {
                StatementNode::Expression(exp_stmt) => {
                    match exp_stmt.expression.as_ref().unwrap() {
                        ExpressionNode::Function(fn_lit) => {
                            assert_eq!(
                                fn_lit.parameters.len(),
                                test.1.len(),
                                "function literal parameters wrong. Expected 2 got={}",
                                fn_lit.parameters.len()
                            );

                            for (idx, ident) in test.1.into_iter().enumerate() {
                                assert_eq!(
                                    fn_lit.parameters[idx].value, ident,
                                    "expected {}, got {}",
                                    ident, fn_lit.parameters[idx].value
                                );
                                assert_eq!(
                                    fn_lit.parameters[idx].token_literal(),
                                    ident,
                                    "expected {}, got {}",
                                    ident,
                                    fn_lit.parameters[idx].token_literal()
                                );
                            }
                        }
                        other => panic!("Expression is not FunctionLiteral. got={:?}", other),
                    }
                }
                other => panic!("statement is not an ExpressionStatement. got={:?}", other),
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap();
        check_parser_errors(parser);

        assert_eq!(
            program.statements.len(),
            1,
            "statements does not contain 1 statements, got={}",
            program.statements.len()
        );

        match &program.statements[0] {
            StatementNode::Expression(exp_stmt) => match exp_stmt.expression.as_ref().unwrap() {
                ExpressionNode::Call(call_exp) => {
                    test_identifier(&call_exp.function, String::from("add"));
                    assert_eq!(
                        call_exp.arguments.len(),
                        3,
                        "wrong length of arguments. got={}",
                        call_exp.arguments.len()
                    );
                    test_literal_expression(&call_exp.arguments[0], Box::new(1));
                    test_infix_expression(
                        &call_exp.arguments[1],
                        Box::new(2),
                        String::from("*"),
                        Box::new(3),
                    );
                    test_infix_expression(
                        &call_exp.arguments[2],
                        Box::new(4),
                        String::from("+"),
                        Box::new(5),
                    );
                }
                other => panic!("Expression is not FunctionLiteral. got={:?}", other),
            },
            other => panic!("statement is not an ExpressionStatement. got={:?}", other),
        }
    }

    #[test]
    fn test_string_literal_expression() {
        let input = r#""hello world""#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(parser);

        match &program.unwrap().statements[0] {
            StatementNode::Expression(exp_stmt) => {
                match &exp_stmt
                    .expression
                    .as_ref()
                    .expect("error parsing expression")
                {
                    ExpressionNode::StringExp(str_literal) => {
                        assert_eq!(
                            str_literal.value, "hello world",
                            "str_literal value not `hello world` got={}",
                            str_literal.value
                        );
                    }
                    other => panic!("not a string literal. got={:?}", other),
                }
            }
            other => panic!("not an expression statement. got={:?}", other),
        }
    }

    #[test]
    fn test_parsing_array_literal() {
        let input = "[1, 2 * 2, 3 + 3]";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(parser);

        match &program.unwrap().statements[0] {
            StatementNode::Expression(exp_stmt) => {
                match &exp_stmt
                    .expression
                    .as_ref()
                    .expect("error parsing expression")
                {
                    ExpressionNode::Array(array_literal) => {
                        assert_eq!(
                            array_literal.elements.len(),
                            3,
                            "Length of array literal elements not 3. got={}",
                            array_literal.elements.len()
                        );
                        test_integer_literal(&array_literal.elements[0], 1);
                        test_infix_expression(
                            &array_literal.elements[1],
                            Box::new(2),
                            String::from("*"),
                            Box::new(2),
                        );
                        test_infix_expression(
                            &array_literal.elements[2],
                            Box::new(3),
                            String::from("+"),
                            Box::new(3),
                        );
                    }
                    other => panic!("not a array literal. got={:?}", other),
                }
            }
            other => panic!("not an expression statement. got={:?}", other),
        }
    }

    #[test]
    fn test_parsing_index_expressions() {
        let input = "myArray[1 + 1]";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(parser);

        match &program.unwrap().statements[0] {
            StatementNode::Expression(exp_stmt) => {
                match &exp_stmt
                    .expression
                    .as_ref()
                    .expect("error parsing expression")
                {
                    ExpressionNode::Index(index_exp) => {
                        test_identifier(&index_exp.left, String::from("myArray"));
                        test_infix_expression(
                            &index_exp.index,
                            Box::new(1),
                            String::from("+"),
                            Box::new(1),
                        );
                    }
                    other => panic!("not an index expression. got={:?}", other),
                }
            }
            other => panic!("not an expression statement. got={:?}", other),
        }
    }

    #[test]
    fn test_parsing_hash_literals_string_keys() {
        let input = r#"{"one": 1, "two": 2, "three": 3}"#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(parser);

        match &program.unwrap().statements[0] {
            StatementNode::Expression(exp_stmt) => {
                match &exp_stmt
                    .expression
                    .as_ref()
                    .expect("error parsing expression")
                {
                    ExpressionNode::Hash(hash) => {
                        assert_eq!(
                            hash.pairs.len(),
                            3,
                            "hash pairs has wrong length. got={}",
                            hash.pairs.len()
                        );

                        let expected = vec![
                            ("one".to_string(), 1),
                            ("two".to_string(), 2),
                            ("three".to_string(), 3),
                        ];

                        let mut curr_idx: usize = 0;

                        for (_, value) in &hash.pairs {
                            let expected_value = expected[curr_idx].1;
                            test_integer_literal(value, expected_value);
                            curr_idx += 1;
                        }
                    }
                    other => panic!("not an hash literal. got={:?}", other),
                }
            }
            other => panic!("not an expression statement. got={:?}", other),
        }
    }

    #[test]
    fn test_parsing_empty_hash_literal() {
        let input = "{}";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(parser);

        match &program.unwrap().statements[0] {
            StatementNode::Expression(exp_stmt) => {
                match &exp_stmt
                    .expression
                    .as_ref()
                    .expect("error parsing expression")
                {
                    ExpressionNode::Hash(hash) => {
                        assert_eq!(
                            hash.pairs.len(),
                            0,
                            "hash pairs has wrong length. got={}",
                            hash.pairs.len()
                        );
                    }
                    other => panic!("not an hash literal. got={:?}", other),
                }
            }
            other => panic!("not an expression statement. got={:?}", other),
        }
    }

    #[test]
    fn test_parsing_hash_literals_with_expressions() {
        let input = r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(parser);

        match &program.unwrap().statements[0] {
            StatementNode::Expression(exp_stmt) => {
                match &exp_stmt
                    .expression
                    .as_ref()
                    .expect("error parsing expression")
                {
                    ExpressionNode::Hash(hash) => {
                        assert_eq!(
                            hash.pairs.len(),
                            3,
                            "hash pairs has wrong length. got={}",
                            hash.pairs.len()
                        );

                        let expected = vec![
                            ("one".to_string(), (0, "+", 1)),
                            ("two".to_string(), (10, "-", 8)),
                            ("three".to_string(), (15, "/", 5)),
                        ];

                        let mut curr_idx: usize = 0;

                        for (_, value) in &hash.pairs {
                            let expected_value = &expected[curr_idx];
                            test_func_for_key(
                                value,
                                expected_value.1 .0,
                                expected_value.1 .1,
                                expected_value.1 .2,
                            );
                            curr_idx += 1;
                        }
                    }
                    other => panic!("not an hash literal. got={:?}", other),
                }
            }
            other => panic!("not an expression statement. got={:?}", other),
        }
    }

    fn test_func_for_key(exp: &ExpressionNode, left: i64, operator: &str, right: i64) {
        test_infix_expression(exp, Box::new(left), operator.to_string(), Box::new(right));
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
