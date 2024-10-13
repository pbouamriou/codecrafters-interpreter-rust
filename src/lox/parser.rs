use crate::lox::{Ast, AstPrint, Token, TokenType};

use core::fmt;
use std::rc::Rc;
pub struct BinaryExpr {
    pub left: Rc<Expr>,
    pub operator: Rc<Token>,
    pub right: Rc<Expr>,
}

pub struct UnaryExpr {
    pub operator: Rc<Token>,
    pub right: Box<Expr>,
}

pub struct GroupExpr {
    pub expr: Box<Expr>,
}

pub struct PrintStatement {
    pub expr: Expr,
}

pub struct VarStatement {
    pub identifier: Rc<Token>,
    pub expr: Option<Expr>,
}

pub struct GroupAssignment {
    pub identifier: Rc<Token>,
    pub expr: Box<Expr>,
}

pub enum Statement {
    Expr(Expr),
    Print(PrintStatement),
    Var(VarStatement),
    Block(Vec<Statement>),
}

pub enum Expr {
    Primary(Rc<Token>),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Group(GroupExpr),
    Assignment(GroupAssignment),
}

pub struct ParserError {
    pub token: Rc<Token>,
    pub message: String,
}

pub struct LoxAstExpression {
    pub root: Expr,
}

pub struct LoxAstProgram {
    pub statements: Vec<Statement>,
}

pub struct LoxParser {
    current: usize,
    tokens: Vec<Rc<Token>>,
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Expr(expr) => {
                write!(f, "{}", expr)
            }
            Self::Print(expr) => {
                write!(f, "(print {})", expr.expr)
            }
            Self::Var(var_statement) => {
                if let Some(expr) = &var_statement.expr {
                    write!(
                        f,
                        "(var {} {})",
                        var_statement.identifier.evaluate_str(),
                        expr
                    )
                } else {
                    write!(f, "(var {})", var_statement.identifier.evaluate_str())
                }
            }
            Self::Block(statements) => {
                write!(f, "(block ")?;
                for statement in statements.iter() {
                    write!(f, "{}", statement)?;
                }
                write!(f, " )")
            }
        }
    }
}

impl AstPrint for LoxAstExpression {
    fn print(&self) {
        println!("{}", self.root)
    }
}

impl AstPrint for LoxAstProgram {
    fn print(&self) {
        for statement in self.statements.iter() {
            println!("{}", statement)
        }
    }
}

impl LoxParser {
    pub fn new(tokens: Vec<Rc<Token>>) -> Self {
        Self { current: 0, tokens }
    }

    pub fn parse_expression(&mut self) -> Result<Ast, ParserError> {
        match self.expression() {
            Ok(ast) => {
                let expr = LoxAstExpression { root: ast };
                let printable = Rc::new(expr);
                let evaluable = printable.clone();
                Ok(Ast {
                    printable,
                    evaluable,
                })
            }
            Err(x) => Err(x),
        }
    }

    pub fn parse_program(&mut self) -> Result<Ast, ParserError> {
        match self.program() {
            Ok(statements) => {
                let expr = LoxAstProgram { statements };
                let printable = Rc::new(expr);
                let evaluable = printable.clone();
                Ok(Ast {
                    printable,
                    evaluable,
                })
            }
            Err(x) => Err(x),
        }
    }

    fn program(&mut self) -> Result<Vec<Statement>, ParserError> {
        let mut statements = Vec::<Statement>::new();
        while self
            .match_cond(|x| matches!(x.token_type, TokenType::Eof))
            .is_none()
        {
            match self.declaration() {
                Ok(statement) => {
                    statements.push(statement);
                }
                Err(err) => return Err(err),
            }
        }
        Ok(statements)
    }

    fn declaration(&mut self) -> Result<Statement, ParserError> {
        match self.var_decl() {
            Ok(expr) => Ok(expr),
            Err(_) => self.statement(),
        }
    }

    fn var_decl(&mut self) -> Result<Statement, ParserError> {
        if self
            .match_cond(|x| matches!(x.token_type, TokenType::Var))
            .is_some()
        {
            if let Some(token_identifier) =
                self.match_cond(|x| matches!(x.token_type, TokenType::Identifier))
            {
                let mut res_expr = None;
                if self
                    .match_cond(|x| matches!(x.token_type, TokenType::Equal))
                    .is_some()
                {
                    let expr = self.expression();
                    if let Err(err) = expr {
                        return Err(err);
                    } else if let Ok(expr) = expr {
                        res_expr = Some(expr)
                    }
                }
                if self
                    .match_cond(|x| matches!(x.token_type, TokenType::SemiColon))
                    .is_some()
                {
                    Ok(Statement::Var(VarStatement {
                        identifier: token_identifier,
                        expr: res_expr,
                    }))
                } else {
                    Err(ParserError {
                        token: self.peek().unwrap(),
                        message: "Missing semicolon.".to_string(),
                    })
                }
            } else {
                Err(ParserError {
                    token: self.peek().unwrap(),
                    message: "Missing identifier.".to_string(),
                })
            }
        } else {
            Err(ParserError {
                token: self.peek().unwrap(),
                message: "Missing var keyword.".to_string(),
            })
        }
    }

    fn statement(&mut self) -> Result<Statement, ParserError> {
        match self.print_statement() {
            Ok(expr) => Ok(expr),
            Err(_) => match self.expression_statement() {
                Ok(statement) => Ok(statement),
                Err(_) => self.block(),
            },
        }
    }

    fn block(&mut self) -> Result<Statement, ParserError> {
        if self
            .match_cond(|x| matches!(x.token_type, TokenType::LeftBrace))
            .is_some()
        {
            let mut statements = Vec::<Statement>::new();
            while self
                .match_cond(|x| matches!(x.token_type, TokenType::RightBrace))
                .is_none()
            {
                match self.declaration() {
                    Ok(statement) => {
                        statements.push(statement);
                    }
                    Err(err) => {
                        return Err(err);
                    }
                }
            }
            Ok(Statement::Block(statements))
        } else {
            Err(ParserError {
                token: self.peek().unwrap(),
                message: "Missing {.".to_string(),
            })
        }
    }

    fn expression_statement(&mut self) -> Result<Statement, ParserError> {
        match self.expression() {
            Ok(expr) => {
                if self
                    .match_cond(|x| matches!(x.token_type, TokenType::SemiColon))
                    .is_some()
                {
                    Ok(Statement::Expr(expr))
                } else {
                    Err(ParserError {
                        token: self.peek().unwrap(),
                        message: "Missing semicolon.".to_string(),
                    })
                }
            }
            Err(err) => Err(err),
        }
    }

    fn print_statement(&mut self) -> Result<Statement, ParserError> {
        if let Some(token_print) = self.match_cond(|x| matches!(x.token_type, TokenType::Print)) {
            match self.expression() {
                Ok(expr) => {
                    if self
                        .match_cond(|x| matches!(x.token_type, TokenType::SemiColon))
                        .is_some()
                    {
                        Ok(Statement::Print(PrintStatement { expr }))
                    } else {
                        Err(ParserError {
                            token: token_print,
                            message: "Missing semicolon.".to_string(),
                        })
                    }
                }
                Err(err) => Err(err),
            }
        } else {
            Err(ParserError {
                token: self.peek().unwrap(),
                message: "Missing print token.".to_string(),
            })
        }
    }

    fn expression(&mut self) -> Result<Expr, ParserError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParserError> {
        if let Some(token_identifier) = self.match_cond_sequence(&[
            Box::new(|x: Rc<Token>| matches!(x.token_type, TokenType::Identifier)),
            Box::new(|x: Rc<Token>| matches!(x.token_type, TokenType::Equal)),
        ]) {
            let assignment = self.assignment();
            match assignment {
                Err(_) => assignment,
                Ok(expr) => Ok(Expr::Assignment(GroupAssignment {
                    identifier: token_identifier.first().unwrap().clone(),
                    expr: Box::new(expr),
                })),
            }
        } else {
            self.equality()
        }
    }

    fn equality(&mut self) -> Result<Expr, ParserError> {
        match self.condition() {
            Ok(expr) => {
                let mut expr = expr;
                while let Some(token) = self.match_cond(|x| {
                    matches!(x.token_type, TokenType::EqualEqual | TokenType::BangEqual)
                }) {
                    match self.condition() {
                        Ok(right) => {
                            expr = Expr::Binary(BinaryExpr {
                                left: Rc::new(expr),
                                operator: token,
                                right: Rc::new(right),
                            })
                        }
                        Err(err) => return Err(err),
                    }
                }
                Ok(expr)
            }
            Err(err) => Err(err),
        }
    }

    fn condition(&mut self) -> Result<Expr, ParserError> {
        match self.term() {
            Ok(expr) => {
                let mut expr = expr;
                while let Some(token) = self.match_cond(|x| {
                    matches!(
                        x.token_type,
                        TokenType::Greater
                            | TokenType::GreaterEqual
                            | TokenType::Less
                            | TokenType::LessEqual
                    )
                }) {
                    match self.term() {
                        Ok(right) => {
                            expr = Expr::Binary(BinaryExpr {
                                left: Rc::new(expr),
                                operator: token,
                                right: Rc::new(right),
                            })
                        }
                        Err(err) => return Err(err),
                    }
                }
                Ok(expr)
            }
            Err(err) => Err(err),
        }
    }

    fn term(&mut self) -> Result<Expr, ParserError> {
        match self.factor() {
            Ok(expr) => {
                let mut expr = expr;
                while let Some(token) =
                    self.match_cond(|x| matches!(x.token_type, TokenType::Plus | TokenType::Minus))
                {
                    match self.factor() {
                        Ok(right) => {
                            expr = Expr::Binary(BinaryExpr {
                                left: Rc::new(expr),
                                operator: token,
                                right: Rc::new(right),
                            })
                        }
                        Err(err) => return Err(err),
                    }
                }
                Ok(expr)
            }
            Err(err) => Err(err),
        }
    }

    fn factor(&mut self) -> Result<Expr, ParserError> {
        match self.unary() {
            Ok(expr) => {
                let mut expr = expr;
                while let Some(token) =
                    self.match_cond(|x| matches!(x.token_type, TokenType::Slash | TokenType::Star))
                {
                    match self.unary() {
                        Ok(right) => {
                            expr = Expr::Binary(BinaryExpr {
                                left: Rc::new(expr),
                                operator: token,
                                right: Rc::new(right),
                            })
                        }
                        Err(err) => return Err(err),
                    }
                }
                Ok(expr)
            }
            Err(err) => Err(err),
        }
    }

    fn unary(&mut self) -> Result<Expr, ParserError> {
        if let Some(token) =
            self.match_cond(|x| matches!(x.token_type, TokenType::Bang | TokenType::Minus))
        {
            match self.unary() {
                Ok(expr) => Ok(Expr::Unary(UnaryExpr {
                    operator: token,
                    right: Box::new(expr),
                })),
                Err(x) => Err(x),
            }
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Result<Expr, ParserError> {
        if let Some(token) = self.match_cond(|x| {
            matches!(
                x.token_type,
                TokenType::False
                    | TokenType::True
                    | TokenType::Nil
                    | TokenType::Number
                    | TokenType::String
                    | TokenType::Identifier
            )
        }) {
            Ok(Expr::Primary(token))
        } else {
            self.group()
        }
    }

    fn group(&mut self) -> Result<Expr, ParserError> {
        if self
            .match_cond(|x| matches!(x.token_type, TokenType::LeftParent))
            .is_some()
        {
            match self.equality() {
                Ok(expr) => {
                    if self
                        .match_cond(|x| matches!(x.token_type, TokenType::RightParent))
                        .is_some()
                    {
                        Ok(Expr::Group(GroupExpr {
                            expr: Box::new(expr),
                        }))
                    } else {
                        Err(ParserError {
                            token: self.peek().unwrap(),
                            message: "Expect expression.".to_string(),
                        })
                    }
                }
                Err(x) => Err(x),
            }
        } else {
            Err(ParserError {
                token: self.peek().unwrap(),
                message: "Expect expression.".to_string(),
            })
        }
    }

    fn match_cond_sequence(
        &mut self,
        cond_seq: &[Box<dyn Fn(Rc<Token>) -> bool>],
    ) -> Option<Vec<Rc<Token>>> {
        let current_position = self.current;
        let mut result: Option<Vec<Rc<Token>>> = None;
        for cond in cond_seq.iter() {
            if let Some(token) = self.peek() {
                if cond(token.clone()) {
                    self.current += 1;
                    if result.is_none() {
                        result = Some(Vec::new());
                    }
                    if let Some(x) = result.as_mut() {
                        x.push(token.clone());
                    }
                } else {
                    self.current = current_position;
                    return None;
                }
            }
        }
        result
    }

    fn match_cond(&mut self, cond: impl Fn(Rc<Token>) -> bool) -> Option<Rc<Token>> {
        if let Some(token) = self.peek() {
            if cond(token.clone()) {
                self.current += 1;
                return Some(token);
            }
        }
        None
    }

    fn peek(&self) -> Option<Rc<Token>> {
        if self.current >= self.tokens.len() {
            return None;
        }
        let item = self.tokens.get(self.current);
        item.cloned()
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Primary(token) => {
                write!(f, "{}", *token.parse())
            }
            Expr::Group(expr) => {
                write!(f, "(group {})", expr.expr)
            }
            Expr::Unary(expr) => {
                write!(f, "({} {})", expr.operator.parse(), expr.right)
            }

            Expr::Binary(expr) => {
                write!(
                    f,
                    "({} {} {})",
                    expr.operator.parse(),
                    expr.left,
                    expr.right
                )
            }
            Expr::Assignment(expr) => {
                write!(f, "({} {})", expr.identifier.parse(), expr.expr)
            }
        }
    }
}
