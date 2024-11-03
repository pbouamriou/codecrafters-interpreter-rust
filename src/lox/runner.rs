use super::private::{
    BinaryExpr, EvaluationError, Expr, GroupExpr, LoxAstExpression, LoxAstProgram, Statement,
    TokenType, UnaryExpr,
};
use super::EvaluationResult;

use std::rc::Weak;
use std::{cell::RefCell, rc::Rc};

use std::collections::HashMap;

use super::{parser::GroupAssignment, traits::AstEvaluation};

pub struct Scope {
    next_block: Option<RefCell<Rc<RefCell<Scope>>>>,
    previous_block: Option<RefCell<Weak<RefCell<Scope>>>>,
    vars: HashMap<String, EvaluationResult>,
}

pub struct Environment {
    current_scope: RefCell<Rc<RefCell<Scope>>>,
    _root_scope: Rc<RefCell<Scope>>,
}

impl Environment {
    fn new() -> Self {
        let current_scope = Rc::new(RefCell::new(Scope {
            next_block: None,
            previous_block: None,
            vars: HashMap::new(),
        }));
        let _root_scope = current_scope.clone();
        Self {
            current_scope: RefCell::new(current_scope),
            _root_scope,
        }
    }

    pub fn value(&self, variable_name: &str) -> Option<EvaluationResult> {
        let mut current = self.current_scope.borrow().clone();
        loop {
            current = {
                let exists = |x: Rc<RefCell<Scope>>| x.borrow().vars.get(variable_name).cloned();
                match exists(current.clone()) {
                    Some(x) => {
                        return Some(x.clone());
                    }
                    None => {
                        if let Some(parent) = current.borrow().previous_block.clone() {
                            parent.borrow().upgrade().unwrap()
                        } else {
                            break;
                        }
                    }
                }
            }
        }
        None
    }

    fn assign_variable(&mut self, variable_name: &str, value: EvaluationResult) -> bool {
        let mut current = self.current_scope.borrow().clone();
        loop {
            current = {
                let exists = |x: Rc<RefCell<Scope>>| x.borrow().vars.get(variable_name).cloned();
                match exists(current.clone()) {
                    Some(_) => {
                        let assign = |x: Rc<RefCell<Scope>>, y: EvaluationResult| {
                            let mut borrow = x.borrow_mut();
                            let value = borrow.vars.get_mut(variable_name).unwrap();
                            *value = y;
                        };
                        assign(current, value);
                        return true;
                    }
                    None => {
                        if let Some(parent) = current.borrow().previous_block.clone() {
                            parent.borrow().upgrade().unwrap()
                        } else {
                            break;
                        }
                    }
                }
            }
        }
        false
    }

    fn add_variable(&mut self, variable_name: &str, value: EvaluationResult) -> bool {
        if self
            .current_scope
            .borrow()
            .borrow()
            .vars
            .contains_key(variable_name)
        {
            let cell_scope = self.current_scope.borrow();
            let mut scope = cell_scope.borrow_mut();
            let item = scope.vars.get_mut(variable_name).unwrap();
            *item = value;
            true
        } else {
            self.current_scope
                .borrow()
                .borrow_mut()
                .vars
                .insert(variable_name.to_string(), value);
            true
        }
    }

    fn has_parent_scope(&self) -> bool {
        self.current_scope
            .borrow()
            .borrow()
            .previous_block
            .is_some()
    }

    fn leave_scope(&mut self) {
        if self.has_parent_scope() {
            let previous_scope = self
                .current_scope
                .borrow()
                .borrow()
                .previous_block
                .clone()
                .unwrap();
            previous_scope
                .borrow()
                .upgrade()
                .unwrap()
                .borrow_mut()
                .next_block = None;
            let mut current = self.current_scope.borrow_mut();
            *current = previous_scope.borrow().upgrade().unwrap();
        }
    }

    fn enter_scope(&mut self) {
        // current.next = new_scope
        // new_scope.previos = current
        // current = new_scope
        let current_scope = &mut *self.current_scope.borrow_mut();
        let rc_scope = Rc::new(RefCell::new(Scope {
            next_block: None,
            previous_block: Some(RefCell::new(Rc::downgrade(current_scope))),
            vars: HashMap::new(),
        }));
        let scope = RefCell::new(rc_scope.clone());
        current_scope.borrow_mut().next_block = Some(scope);
        *current_scope = rc_scope;
    }
}

impl AstEvaluation for LoxAstExpression {
    fn evaluate(&self) -> EvaluationResult {
        let mut environment = Environment::new();
        self.root.evaluate(&mut environment)
    }
}

impl AstEvaluation for LoxAstProgram {
    fn evaluate(&self) -> EvaluationResult {
        let mut eval = EvaluationResult::Nil;
        let mut environment = Environment::new();
        for statement in self.statements.iter() {
            eval = statement.evaluate(&mut environment);
            if let EvaluationResult::Error(_) = eval {
                return eval;
            }
        }
        eval
    }
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
        environment.assign_variable(expr.identifier.get_lexem(), result.clone());
        result
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
            Self::Block(statements) => {
                environment.enter_scope();
                let mut last_result = EvaluationResult::Nil;
                for statement in statements.iter() {
                    let result = statement.evaluate(environment);
                    last_result = result;
                    if let EvaluationResult::Error(_) = last_result {
                        break;
                    }
                }
                environment.leave_scope();
                last_result
            }
            Self::Var(var_statement) => {
                let mut result = EvaluationResult::Nil;
                if let Some(expr) = &var_statement.expr {
                    result = expr.evaluate(environment);
                    if let EvaluationResult::Error(_) = result {
                        return result;
                    }
                }
                environment.add_variable(var_statement.identifier.get_lexem(), result);
                EvaluationResult::Nil
            }
        }
    }
}
