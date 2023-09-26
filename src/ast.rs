use std::collections::HashMap;

use crate::token::Token;

pub trait Node {
    fn token_literal(&self) -> String;
    fn print_string(&self) -> String;
}

#[derive(Debug, Clone)]
pub enum StatementNode {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    Block(BlockStatement),
}

impl Node for StatementNode {
    fn token_literal(&self) -> String {
        return match self {
            Self::Let(let_stmt) => let_stmt.token_literal(),
            Self::Return(ret_stmt) => ret_stmt.token_literal(),
            Self::Expression(expression) => expression.token_literal(),
            Self::Block(block_stmt) => block_stmt.token_literal(),
        };
    }

    fn print_string(&self) -> String {
        return match self {
            Self::Let(let_stmt) => let_stmt.print_string(),
            Self::Return(ret_stmt) => ret_stmt.print_string(),
            Self::Expression(expression) => expression.print_string(),
            Self::Block(block_stmt) => block_stmt.print_string(),
        };
    }
}

#[derive(Debug, Default, Clone)]
pub enum ExpressionNode {
    #[default]
    None,
    IdentifierNode(Identifier),
    Integer(IntegerLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    BooleanNode(Boolean),
    IfExpressionNode(IfExpression),
    Function(FunctionLiteral),
    Call(CallExpression),
    StringExp(StringLiteral),
    Array(ArrayLiteral),
    Index(IndexExpression),
    Hash(HashLiteral),
}

impl Node for ExpressionNode {
    fn token_literal(&self) -> String {
        return match self {
            Self::IdentifierNode(identifier) => identifier.token_literal(),
            Self::Integer(integer) => integer.token_literal(),
            Self::Prefix(prefix_exp) => prefix_exp.token_literal(),
            Self::Infix(infix_exp) => infix_exp.token_literal(),
            Self::BooleanNode(bool_exp) => bool_exp.token_literal(),
            Self::IfExpressionNode(if_exp) => if_exp.token_literal(),
            Self::Function(func_literal) => func_literal.token_literal(),
            Self::Call(call_exp) => call_exp.token_literal(),
            Self::StringExp(string) => string.token_literal(),
            Self::Array(array) => array.token_literal(),
            Self::Index(index_exp) => index_exp.token_literal(),
            Self::Hash(hash) => hash.token_literal(),
            Self::None => String::from(""),
        };
    }

    fn print_string(&self) -> String {
        return match self {
            Self::IdentifierNode(identifier) => identifier.print_string(),
            Self::Integer(integer) => integer.print_string(),
            Self::Prefix(prefix_exp) => prefix_exp.print_string(),
            Self::Infix(infix_exp) => infix_exp.print_string(),
            Self::BooleanNode(bool_exp) => bool_exp.print_string(),
            Self::IfExpressionNode(if_exp) => if_exp.print_string(),
            Self::Function(func_literal) => func_literal.print_string(),
            Self::Call(call_exp) => call_exp.print_string(),
            Self::StringExp(string) => string.print_string(),
            Self::Array(array) => array.print_string(),
            Self::Index(index_exp) => index_exp.print_string(),
            Self::Hash(hash) => hash.print_string(),
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
                StatementNode::Block(block_stmt) => block_stmt.token_literal(),
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Default, Clone)]
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

#[derive(Debug, Default, Clone)]
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

#[derive(Debug, Default, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Default, Clone)]
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

#[derive(Debug, Clone)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

impl Node for Boolean {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn print_string(&self) -> String {
        self.token_literal()
    }
}

#[derive(Debug, Default, Clone)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<ExpressionNode>,
    pub operator: String,
    pub right: Box<ExpressionNode>,
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn print_string(&self) -> String {
        let mut out = String::from("");
        out.push_str("(");
        out.push_str(self.left.print_string().as_str());
        out.push_str(format!(" {} ", self.operator).as_str());
        out.push_str(self.right.print_string().as_str());
        out.push_str(")");

        out
    }
}

#[derive(Debug, Default, Clone)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<ExpressionNode>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Node for IfExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn print_string(&self) -> String {
        let mut out = String::from("");

        out.push_str("if");
        out.push_str(self.condition.print_string().as_str());
        out.push_str(" ");
        out.push_str(self.consequence.print_string().as_str());

        if let Some(alt) = &self.alternative {
            out.push_str("else ");
            out.push_str(alt.print_string().as_str());
        }

        out
    }
}

#[derive(Debug, Default, Clone)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn print_string(&self) -> String {
        let mut out = String::from("");
        let mut params = vec![];

        for param in &self.parameters {
            params.push(param.print_string());
        }

        out.push_str(self.token_literal().as_str());
        out.push_str("(");
        out.push_str(params.join(", ").as_str());
        out.push_str(")");
        out.push_str(self.body.print_string().as_str());

        out
    }
}

#[derive(Debug, Default, Clone)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<ExpressionNode>,
    pub arguments: Vec<ExpressionNode>,
}

impl Node for CallExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn print_string(&self) -> String {
        let mut out = String::from("");
        let mut args = vec![];

        for arg in &self.arguments {
            args.push(arg.print_string());
        }

        out.push_str(self.function.print_string().as_str());
        out.push_str("(");
        out.push_str(args.join(", ").as_str());
        out.push_str(")");

        out
    }
}

#[derive(Debug, Default, Clone)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<StatementNode>,
}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn print_string(&self) -> String {
        let mut out = String::from("");

        for stmt in &self.statements {
            out.push_str(stmt.print_string().as_str());
        }

        out
    }
}

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

impl Node for StringLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn print_string(&self) -> String {
        self.token_literal()
    }
}

#[derive(Debug, Clone)]
pub struct ArrayLiteral {
    pub token: Token, //[
    pub elements: Vec<ExpressionNode>,
}

impl Node for ArrayLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn print_string(&self) -> String {
        let mut out = String::from("");
        let mut elements = vec![];

        for el in &self.elements {
            elements.push(el.print_string());
        }

        out.push_str("[");
        out.push_str(elements.join(", ").as_str());
        out.push_str("]");

        out
    }
}

#[derive(Debug, Clone)]
pub struct IndexExpression {
    pub token: Token, // [
    pub left: Box<ExpressionNode>,
    pub index: Box<ExpressionNode>,
}

impl Node for IndexExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn print_string(&self) -> String {
        let mut out = String::from("");

        out.push_str("(");
        out.push_str(self.left.print_string().as_str());
        out.push_str("[");
        out.push_str(self.index.print_string().as_str());
        out.push_str("])");

        out
    }
}

#[derive(Debug, Clone)]
pub struct HashLiteral {
    pub token: Token, // {
    pub pairs: HashMap<ExpressionNode, ExpressionNode>,
}

impl Node for HashLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn print_string(&self) -> String {
        let mut out = String::from("");
        let mut pairs = vec![];

        for (key, value) in &self.pairs {
            pairs.push(format!("{}:{}", key.print_string(), value.print_string()))
        }

        out.push_str("{");
        out.push_str(pairs.join(", ").as_str());
        out.push_str("}");

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
