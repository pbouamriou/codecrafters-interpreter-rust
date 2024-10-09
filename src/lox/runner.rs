use crate::lox::{
    BinaryExpr, EvaluationError, EvaluationResult, Expr, GroupExpr, LoxAstExpression,
    LoxAstProgram, Statement, TokenType, UnaryExpr,
};

use std::collections::HashMap;

use super::{parser::GroupAssignment, traits::AstEvaluation};

impl AstEvaluation for LoxAstExpression {
    fn evaluate(&self) -> EvaluationResult {
        let mut environment = Environment {
            global_vars: HashMap::new(),
        };
        self.root.evaluate(&mut environment)
    }
}

impl AstEvaluation for LoxAstProgram {
    fn evaluate(&self) -> EvaluationResult {
        let mut eval = EvaluationResult::Nil;
        let mut environment = Environment {
            global_vars: HashMap::new(),
        };
        for statement in self.statements.iter() {
            eval = statement.evaluate(&mut environment);
            if let EvaluationResult::Error(_) = eval {
                return eval;
            }
        }
        eval
    }
}

pub struct Environment {
    pub global_vars: HashMap<String, EvaluationResult>,
}

impl Expr {
    pub fn evaluate(&self, environment: &mut Environment) -> EvaluationResult {
        match self {
            Expr::Primary(token) => token.evaluate(environment),
            Expr::Unary(expr) => Self::evaluate_unary(expr, environment),
            Expr::Group(expr) => Self::evaluate_group(expr, environment),
            Expr::Binary(expr) => Self::evaluate_binary(expr, environment),
            Expr::Assignment(expr) => Self::evaluate_assignment(expr, environment),
        }
    }

    fn evaluate_assignment(
        expr: &GroupAssignment,
        environment: &mut Environment,
    ) -> EvaluationResult {
        let result = expr.expr.evaluate(environment);
        match environment.global_vars.get_mut(expr.identifier.get_lexem()) {
            None => {
                if let EvaluationResult::Error(_) = result {
                } else {
                    environment
                        .global_vars
                        .insert(expr.identifier.get_lexem().clone(), result.clone());
                }
                result
            }
            Some(item) => {
                if let EvaluationResult::Error(_) = result {
                } else {
                    *item = result.clone();
                }
                result
            }
        }
    }

    fn evaluate_unary(expr: &UnaryExpr, environment: &mut Environment) -> EvaluationResult {
        match expr.operator.token_type {
            TokenType::Bang => match expr.right.evaluate(environment) {
                EvaluationResult::Boolean(x) => EvaluationResult::Boolean(!x),
                EvaluationResult::Nil => EvaluationResult::Boolean(true),
                EvaluationResult::Number(x) => EvaluationResult::Boolean(x == 0 as f64),

                _ => EvaluationResult::Error(EvaluationError {
                    token: expr.operator.clone(),
                    message: "Operand must be a number, boolean, or nil".to_string(),
                }),
            },
            TokenType::Minus => match expr.right.evaluate(environment) {
                EvaluationResult::Number(x) => EvaluationResult::Number(-x),
                _ => EvaluationResult::Error(EvaluationError {
                    token: expr.operator.clone(),
                    message: "Operand must be a number".to_string(),
                }),
            },
            TokenType::Plus => match expr.right.evaluate(environment) {
                EvaluationResult::Number(x) => EvaluationResult::Number(x),
                _ => EvaluationResult::Error(EvaluationError {
                    token: expr.operator.clone(),
                    message: "Operand must be a number".to_string(),
                }),
            },
            _ => EvaluationResult::Error(EvaluationError {
                token: expr.operator.clone(),
                message: "unsupported token_type for unary evaluation".to_string(),
            }),
        }
    }

    fn evaluate_binary(expr: &BinaryExpr, environment: &mut Environment) -> EvaluationResult {
        let evaluation = (
            expr.right.evaluate(environment),
            expr.left.evaluate(environment),
        );
        let token_type = expr.operator.token_type;
        match token_type {
            TokenType::Minus | TokenType::Slash | TokenType::Star => {
                if let (EvaluationResult::Number(right), EvaluationResult::Number(left)) =
                    evaluation
                {
                    if let Some(operation) = Self::get_float_operation(token_type) {
                        EvaluationResult::Number(operation(left, right))
                    } else {
                        EvaluationResult::Error(EvaluationError {
                            token: expr.operator.clone(),
                            message: "Operands must be numbers.".to_string(),
                        })
                    }
                } else {
                    EvaluationResult::Error(EvaluationError {
                        token: expr.operator.clone(),
                        message: "Operands must be numbers.".to_string(),
                    })
                }
            }
            TokenType::Plus => {
                if let (EvaluationResult::Number(right), EvaluationResult::Number(left)) =
                    evaluation
                {
                    if let Some(operation) = Self::get_float_operation(token_type) {
                        EvaluationResult::Number(operation(left, right))
                    } else {
                        EvaluationResult::Error(EvaluationError {
                            token: expr.operator.clone(),
                            message: "Operands must be numbers.".to_string(),
                        })
                    }
                } else if let (EvaluationResult::Str(right), EvaluationResult::Str(left)) =
                    evaluation
                {
                    if let Some(operation) = Self::get_str_operation(token_type) {
                        EvaluationResult::Str(operation(left, right))
                    } else {
                        EvaluationResult::Error(EvaluationError {
                            token: expr.operator.clone(),
                            message: "Operands must be numbers.".to_string(),
                        })
                    }
                } else {
                    EvaluationResult::Error(EvaluationError {
                        token: expr.operator.clone(),
                        message: "Operands must be numbers or strings.".to_string(),
                    })
                }
            }
            TokenType::LessEqual
            | TokenType::Less
            | TokenType::Greater
            | TokenType::GreaterEqual => {
                if let (EvaluationResult::Number(right), EvaluationResult::Number(left)) =
                    evaluation
                {
                    if let Some(operation) = Self::get_float_comparison(token_type) {
                        EvaluationResult::Boolean(operation(left, right))
                    } else {
                        EvaluationResult::Error(EvaluationError {
                            token: expr.operator.clone(),
                            message: "Operands must be numbers.".to_string(),
                        })
                    }
                } else if let (EvaluationResult::Str(right), EvaluationResult::Str(left)) =
                    evaluation
                {
                    if let Some(operation) = Self::get_str_comparison(token_type) {
                        EvaluationResult::Boolean(operation(left, right))
                    } else {
                        EvaluationResult::Error(EvaluationError {
                            token: expr.operator.clone(),
                            message: "Operands must be numbers.".to_string(),
                        })
                    }
                } else {
                    EvaluationResult::Error(EvaluationError {
                        token: expr.operator.clone(),
                        message: "Operands must be numbers or strings.".to_string(),
                    })
                }
            }
            TokenType::EqualEqual | TokenType::BangEqual => {
                let mut evaluation = evaluation;
                if let EvaluationResult::Nil = evaluation.0 {
                    evaluation.0 = EvaluationResult::Boolean(false);
                }
                if let EvaluationResult::Nil = evaluation.1 {
                    evaluation.1 = EvaluationResult::Boolean(false);
                }
                if Self::is_different_type(&evaluation) {
                    match token_type {
                        TokenType::EqualEqual => EvaluationResult::Boolean(false),
                        TokenType::BangEqual => EvaluationResult::Boolean(true),
                        _ => EvaluationResult::Boolean(false),
                    }
                } else if let (EvaluationResult::Number(right), EvaluationResult::Number(left)) =
                    evaluation
                {
                    if let Some(operation) = Self::get_float_comparison(token_type) {
                        EvaluationResult::Boolean(operation(left, right))
                    } else {
                        EvaluationResult::Error(EvaluationError {
                            token: expr.operator.clone(),
                            message: "Operands must be numbers.".to_string(),
                        })
                    }
                } else if let (EvaluationResult::Str(right), EvaluationResult::Str(left)) =
                    evaluation
                {
                    if let Some(operation) = Self::get_str_comparison(token_type) {
                        EvaluationResult::Boolean(operation(left, right))
                    } else {
                        EvaluationResult::Error(EvaluationError {
                            token: expr.operator.clone(),
                            message: "Operands must be numbers.".to_string(),
                        })
                    }
                } else if let (EvaluationResult::Boolean(right), EvaluationResult::Boolean(left)) =
                    evaluation
                {
                    if let Some(operation) = Self::get_bool_comparison(token_type) {
                        EvaluationResult::Boolean(operation(left, right))
                    } else {
                        EvaluationResult::Error(EvaluationError {
                            token: expr.operator.clone(),
                            message: "Operands must be numbers.".to_string(),
                        })
                    }
                } else {
                    EvaluationResult::Error(EvaluationError {
                        token: expr.operator.clone(),
                        message: "Operands must be numbers, strings, bool or nil.".to_string(),
                    })
                }
            }
            _ => EvaluationResult::Error(EvaluationError {
                token: expr.operator.clone(),
                message: "unsupported token_type for binary evaluation".to_string(),
            }),
        }
    }

    fn is_different_type(evaluation: &(EvaluationResult, EvaluationResult)) -> bool {
        let (x, y) = evaluation;
        match x {
            EvaluationResult::Boolean(_) => !matches!(y, EvaluationResult::Boolean(_)),
            EvaluationResult::Str(_) => !matches!(y, EvaluationResult::Str(_)),
            EvaluationResult::Nil => !matches!(y, EvaluationResult::Nil),
            EvaluationResult::Number(_) => !matches!(y, EvaluationResult::Number(_)),
            _ => true,
        }
    }

    fn get_float_operation(token_type: TokenType) -> Option<&'static dyn Fn(f64, f64) -> f64> {
        match token_type {
            TokenType::Slash => Some(&|x: f64, y: f64| x / y),
            TokenType::Star => Some(&|x: f64, y: f64| x * y),
            TokenType::Plus => Some(&|x: f64, y: f64| x + y),
            TokenType::Minus => Some(&|x: f64, y: f64| x - y),
            _ => None,
        }
    }

    fn get_str_operation(
        token_type: TokenType,
    ) -> Option<&'static dyn Fn(String, String) -> String> {
        match token_type {
            TokenType::Plus => Some(&|x, y| {
                let mut result = String::new();
                result.push_str(x.as_str());
                result.push_str(y.as_str());
                result
            }),
            _ => None,
        }
    }

    fn get_bool_comparison(token_type: TokenType) -> Option<&'static dyn Fn(bool, bool) -> bool> {
        match token_type {
            TokenType::EqualEqual => Some(&|x, y| x == y),
            TokenType::BangEqual => Some(&|x, y| x != y),
            _ => None,
        }
    }

    fn get_float_comparison(token_type: TokenType) -> Option<&'static dyn Fn(f64, f64) -> bool> {
        match token_type {
            TokenType::EqualEqual => Some(&|x, y| x == y),
            TokenType::BangEqual => Some(&|x, y| x != y),
            TokenType::Less => Some(&|x, y| x < y),
            TokenType::LessEqual => Some(&|x, y| x <= y),
            TokenType::Greater => Some(&|x, y| x > y),
            TokenType::GreaterEqual => Some(&|x, y| x >= y),
            _ => None,
        }
    }

    fn get_str_comparison(
        token_type: TokenType,
    ) -> Option<&'static dyn Fn(String, String) -> bool> {
        match token_type {
            TokenType::EqualEqual => Some(&|x, y| x == y),
            TokenType::BangEqual => Some(&|x, y| x != y),
            TokenType::Less => Some(&|x, y| x < y),
            TokenType::LessEqual => Some(&|x, y| x <= y),
            TokenType::Greater => Some(&|x, y| x > y),
            TokenType::GreaterEqual => Some(&|x, y| x >= y),
            _ => None,
        }
    }

    fn evaluate_group(expr: &GroupExpr, environment: &mut Environment) -> EvaluationResult {
        expr.expr.evaluate(environment)
    }
}

impl Statement {
    fn evaluate(&self, environment: &mut Environment) -> EvaluationResult {
        match self {
            Self::Expr(expr) => expr.evaluate(environment),
            Self::Print(statement) => {
                let result = statement.expr.evaluate(environment);
                if let EvaluationResult::Error(_) = result {
                    result
                } else {
                    println!("{}", result);
                    result
                }
            }
            Self::Var(var_statement) => {
                match environment
                    .global_vars
                    .get_mut(var_statement.identifier.get_lexem())
                {
                    None => {
                        if let Some(expr) = &var_statement.expr {
                            let result = expr.evaluate(environment);
                            if let EvaluationResult::Error(_) = result {
                                return result;
                            } else {
                                environment
                                    .global_vars
                                    .insert(var_statement.identifier.get_lexem().clone(), result);
                            }
                        } else {
                            environment.global_vars.insert(
                                var_statement.identifier.get_lexem().clone(),
                                EvaluationResult::Nil,
                            );
                        }
                        EvaluationResult::Nil
                    }
                    Some(item) => {
                        if let Some(expr) = &var_statement.expr {
                            let result = expr.evaluate(environment);
                            if let EvaluationResult::Error(_) = result {
                                return result;
                            } else {
                                let item = environment
                                    .global_vars
                                    .get_mut(var_statement.identifier.get_lexem())
                                    .unwrap();
                                *item = result;
                            }
                        } else {
                            *item = EvaluationResult::Nil;
                        }
                        EvaluationResult::Nil
                    }
                }
            }
        }
    }
}
