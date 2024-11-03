mod lox;

use lox::EvaluationResult;
use lox::LoxParser;
use lox::LoxScanner;

use std::env;
use std::fs;
use std::process::ExitCode;
use std::process::Termination;

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
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        return AppExitCode::Err(ApplicationErrorCode::WrongNumberOfParameters);
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            // You can use print statements as follows for debugging, they'll be visible when running tests.
            eprintln!("Logs from your program will appear here!");

            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });

            let mut scanner = LoxScanner::new(&file_contents);

            match scanner.tokenize() {
                Ok(tokens) => {
                    for token in tokens.iter() {
                        println!("{}", token)
                    }
                    AppExitCode::Ok
                }
                Err(tokens) => {
                    for token in tokens.iter() {
                        println!("{}", token)
                    }
                    AppExitCode::Err(ApplicationErrorCode::UnexpectedCaracter)
                }
            }
        }
        "parse" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });

            let mut scanner = LoxScanner::new(&file_contents);

            match scanner.tokenize() {
                Ok(tokens) => {
                    let mut parser = LoxParser::new(tokens);
                    match parser.parse_expression() {
                        Err(err) => {
                            eprintln!(
                                "[line {}] Error at '{}': {}",
                                err.token.get_position().line_number,
                                err.token.get_lexem(),
                                err.message
                            );
                            return AppExitCode::Err(ApplicationErrorCode::UnexpectedCaracter);
                        }
                        Ok(ast) => ast.printable.print(),
                    }
                    AppExitCode::Ok
                }
                Err(_) => AppExitCode::Err(ApplicationErrorCode::UnexpectedCaracter),
            }
        }

        "evaluate" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });

            let mut scanner = LoxScanner::new(&file_contents);

            match scanner.tokenize() {
                Ok(tokens) => {
                    let mut parser = LoxParser::new(tokens);
                    match parser.parse_expression() {
                        Err(err) => {
                            eprintln!(
                                "[line {}] Error at '{}': {}",
                                err.token.get_position().line_number,
                                err.token.get_lexem(),
                                err.message
                            );
                            return AppExitCode::Err(ApplicationErrorCode::UnexpectedCaracter);
                        }
                        Ok(ast) => {
                            let result = ast.evaluable.evaluate();
                            match result {
                                EvaluationResult::Error(_) => {
                                    eprintln!("{}", result);
                                    return AppExitCode::Err(ApplicationErrorCode::EvaluationError);
                                }
                                _ => {
                                    println!("{}", result);
                                }
                            }
                        }
                    }
                    AppExitCode::Ok
                }
                Err(_) => AppExitCode::Err(ApplicationErrorCode::UnexpectedCaracter),
            }
        }
        "run" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });

            let mut scanner = LoxScanner::new(&file_contents);

            match scanner.tokenize() {
                Ok(tokens) => {
                    let mut parser = LoxParser::new(tokens);
                    match parser.parse_program() {
                        Err(err) => {
                            eprintln!(
                                "[line {}] Error at '{}': {}",
                                err.token.get_position().line_number,
                                err.token.get_lexem(),
                                err.message
                            );
                            return AppExitCode::Err(ApplicationErrorCode::UnexpectedCaracter);
                        }
                        Ok(ast) => {
                            let result = ast.evaluable.evaluate();
                            match result {
                                EvaluationResult::Error(err) => {
                                    eprintln!(
                                        "[line {}] Error at '{}': {}",
                                        err.token.get_position().line_number,
                                        err.token.get_lexem(),
                                        err.message
                                    );
                                    return AppExitCode::Err(ApplicationErrorCode::EvaluationError);
                                }
                                _ => {
                                    eprintln!("{}", result);
                                }
                            }
                        }
                    }
                    AppExitCode::Ok
                }
                Err(_) => AppExitCode::Err(ApplicationErrorCode::UnexpectedCaracter),
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command);
            AppExitCode::Err(ApplicationErrorCode::UnknownCommand)
        }
    }
}
