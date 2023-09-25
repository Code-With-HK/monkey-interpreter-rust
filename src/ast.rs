use crate::token::Token;

pub trait Node {
    fn token_literal(&self) -> String;
    fn print_string(&self) -> String;
}

#[derive(Debug)]
pub enum StatementNode {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

impl Node for StatementNode {
    fn token_literal(&self) -> String {
        return match self {
            Self::Let(let_stmt) => let_stmt.token_literal(),
            Self::Return(ret_stmt) => ret_stmt.token_literal(),
            Self::Expression(expression) => expression.token_literal(),
        };
    }

    fn print_string(&self) -> String {
        return match self {
            Self::Let(let_stmt) => let_stmt.print_string(),
            Self::Return(ret_stmt) => ret_stmt.print_string(),
            Self::Expression(expression) => expression.print_string(),
        };
    }
}

#[derive(Debug, Default)]
pub enum ExpressionNode {
    #[default]
    None,
    IdentifierNode(Identifier),
    Integer(IntegerLiteral),
    Prefix(PrefixExpression),
}

impl Node for ExpressionNode {
    fn token_literal(&self) -> String {
        return match self {
            Self::IdentifierNode(identifier) => identifier.token_literal(),
            Self::Integer(integer) => integer.token_literal(),
            Self::Prefix(prefix_exp) => prefix_exp.token_literal(),
            Self::None => String::from(""),
        };
    }

    fn print_string(&self) -> String {
        return match self {
            Self::IdentifierNode(identifier) => identifier.print_string(),
            Self::Integer(integer) => integer.print_string(),
            Self::Prefix(prefix_exp) => prefix_exp.print_string(),
            Self::None => String::from(""),
        };
    }
}

pub struct Program {
    pub statements: Vec<StatementNode>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
        return if self.statements.len() > 0 {
            match &self.statements[0] {
                StatementNode::Let(let_stmt) => let_stmt.token_literal(),
                StatementNode::Return(ret_stmt) => ret_stmt.token_literal(),
                StatementNode::Expression(expression) => expression.token_literal(),
            }
        } else {
            String::from("")
        };
    }

    fn print_string(&self) -> String {
        let mut out = String::from("");

        for stmt in self.statements.as_slice() {
            out.push_str(stmt.print_string().as_str());
        }

        out
    }
}

#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Option<ExpressionNode>,
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn print_string(&self) -> String {
        let mut out = String::from("");

        out.push_str(self.token_literal().as_str());
        out.push_str(" ");
        out.push_str(self.name.print_string().as_str());
        out.push_str(" = ");

        if let Some(value) = &self.value {
            out.push_str(value.print_string().as_str());
        }
        out.push_str(";");

        out
    }
}

#[derive(Debug, Default)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn print_string(&self) -> String {
        self.value.clone()
    }
}

#[derive(Debug, Default)]
pub struct ReturnStatement {
    pub token: Token,
    pub ret_value: Option<ExpressionNode>,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn print_string(&self) -> String {
        let mut out = String::from("");

        out.push_str(self.token_literal().as_str());
        out.push_str(" ");

        if let Some(ret_value) = &self.ret_value {
            out.push_str(ret_value.print_string().as_str());
        }

        out.push_str(";");

        out
    }
}

#[derive(Debug, Default)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<ExpressionNode>,
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn print_string(&self) -> String {
        if let Some(expression) = &self.expression {
            return expression.print_string();
        }
        String::from("")
    }
}

#[derive(Debug)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn print_string(&self) -> String {
        self.token_literal()
    }
}

#[derive(Debug, Default)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<ExpressionNode>,
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn print_string(&self) -> String {
        let mut out = String::from("");
        out.push_str("(");
        out.push_str(self.operator.as_str());
        out.push_str(self.right.print_string().as_str());
        out.push_str(")");

        out
    }
}

#[cfg(test)]
mod test {
    use crate::{
        ast::Node,
        token::{Token, TokenKind},
    };

    use super::{ExpressionNode, Identifier, LetStatement, Program, StatementNode};

    #[test]
    fn test_print_string() {
        let program = Program {
            statements: vec![StatementNode::Let(LetStatement {
                token: Token {
                    kind: TokenKind::Let,
                    literal: String::from("let"),
                },
                name: Identifier {
                    token: Token {
                        kind: TokenKind::Ident,
                        literal: String::from("myVar"),
                    },
                    value: String::from("myVar"),
                },
                value: Some(ExpressionNode::IdentifierNode(Identifier {
                    token: Token {
                        kind: TokenKind::Ident,
                        literal: String::from("anotherVar"),
                    },
                    value: String::from("anotherVar"),
                })),
            })],
        };

        assert_eq!(
            program.print_string(),
            String::from("let myVar = anotherVar;"),
            "print string wrong. got = {}",
            program.print_string()
        );
    }
}
