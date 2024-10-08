mod lox;

use lox::traits::EvaluationResult;

use crate::lox::{LoxParser, LoxScanner};
use crate::lox::traits::Ast;

use std::env;
use std::fs;
use std::process::Termination;
use std::process::ExitCode;
use std::io::{self, Write};

enum ApplicationErrorCode {
    UnexpectedCaracter = 65,
    EvaluationError = 70,
    WrongNumberOfParameters = 1,
    UnknownCommand = 2,
}
enum AppExitCode {
    Ok,
    Err(ApplicationErrorCode),
}


impl Termination for AppExitCode {
    fn report(self) -> ExitCode {
        match self {
            AppExitCode::Ok => ExitCode::SUCCESS,
            AppExitCode::Err(v) => ExitCode::from(v as u8),
        }
    }
}


fn main() -> AppExitCode {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} tokenize <filename>", args[0]).unwrap();
        return AppExitCode::Err(ApplicationErrorCode::WrongNumberOfParameters)
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            // You can use print statements as follows for debugging, they'll be visible when running tests.
            writeln!(io::stderr(), "Logs from your program will appear here!").unwrap();

            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            let mut scanner = LoxScanner::new(&file_contents);

            match scanner.tokenize() {
                Ok(tokens) => {
                    for token in tokens.iter() {
                        println!("{}", token)
                    }
                    return AppExitCode::Ok;
                },
                Err(tokens) => {
                    for token in tokens.iter() {
                        println!("{}", token)
                    }
                    return AppExitCode::Err(ApplicationErrorCode::UnexpectedCaracter);
                }
            }
        }
        "parse" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            let mut scanner = LoxScanner::new(&file_contents);

            match scanner.tokenize() {
                Ok(tokens) => {
                    let mut parser = LoxParser::new(tokens);
                    match parser.parse_expression() {
                        Err(err) => {
                            writeln!(io::stderr(), "[line {}] Error at '{}': {}", err.token.get_position().line_number, err.token.get_lexem(), err.message).unwrap();
                            return AppExitCode::Err(ApplicationErrorCode::UnexpectedCaracter)
                        }
                        Ok(ast) => ast.print(),
                    }
                    return AppExitCode::Ok
                }
                Err(_) => return AppExitCode::Err(ApplicationErrorCode::UnexpectedCaracter)
            }
        }

        "evaluate" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            let mut scanner = LoxScanner::new(&file_contents);

            match scanner.tokenize() {
                Ok(tokens) => {
                    let mut parser = LoxParser::new(tokens);
                    match parser.parse_expression() {
                        Err(err) => {
                            writeln!(io::stderr(), "[line {}] Error at '{}': {}", err.token.get_position().line_number, err.token.get_lexem(), err.message).unwrap();
                            return AppExitCode::Err(ApplicationErrorCode::UnexpectedCaracter)
                        }
                        Ok(ast) => {
                            let result = ast.evaluate();
                            match result {
                                EvaluationResult::Error(_) => {
                                    writeln!(io::stderr(), "{}", result).unwrap();
                                    return AppExitCode::Err(ApplicationErrorCode::EvaluationError)
                                }
                                _ => { println!("{}", result); }
                            }
                        }
                    }
                    return AppExitCode::Ok
                }
                Err(_) => return AppExitCode::Err(ApplicationErrorCode::UnexpectedCaracter)
            }
        }
        "run" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            let mut scanner = LoxScanner::new(&file_contents);

            match scanner.tokenize() {
                Ok(tokens) => {
                    let mut parser = LoxParser::new(tokens);
                    match parser.parse_program() {
                        Err(err) => {
                            writeln!(io::stderr(), "[line {}] Error at '{}': {}", err.token.get_position().line_number, err.token.get_lexem(), err.message).unwrap();
                            return AppExitCode::Err(ApplicationErrorCode::UnexpectedCaracter)
                        }
                        Ok(ast) => {
                            let result = ast.evaluate();
                            match result {
                                EvaluationResult::Error(_) => {
                                    writeln!(io::stderr(), "{}", result).unwrap();
                                    return AppExitCode::Err(ApplicationErrorCode::EvaluationError)
                                }
                                _ => { writeln!(io::stderr(),"{}", result).unwrap(); }
                            }
                        }
                    }
                    return AppExitCode::Ok
                }
                Err(_) => return AppExitCode::Err(ApplicationErrorCode::UnexpectedCaracter)
            }
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return AppExitCode::Err(ApplicationErrorCode::UnknownCommand);
        }
    }
}
