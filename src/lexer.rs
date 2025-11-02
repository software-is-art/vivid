use std::str::Chars;

use crate::diagnostics::{Diagnostic, DiagnosticKind};
use crate::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Identifier(String),
    Int(i64),
    Float(f64),
    String(String),
    Keyword(Keyword),
    Symbol(Symbol),
    Eof,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Keyword {
    Value,
    Struct,
    Fn,
    Source,
    Sink,
    Normalize,
    Require,
    Always,
    Policy,
    Invalid,
    Error,
    Drop,
    Hold,
    Replace,
    When,
    Upon,
    Next,
    First,
    Rest,
    Defined,
    Let,
    In,
    If,
    Else,
    True,
    False,
    From,
    To,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Symbol {
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Colon,
    Semicolon,
    Dot,
    Arrow,
    FatArrow,
    Equal,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    DoubleEqual,
    NotEqual,
    Bang,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    And,
    Or,
    Pipe,
}

pub fn lex(input: &str) -> Result<Vec<Token>, Vec<Diagnostic>> {
    let lexer = Lexer::new(input);
    lexer.lex()
}

struct Lexer<'a> {
    input: &'a str,
    chars: Chars<'a>,
    current_index: usize,
    next_index: usize,
    current: Option<char>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        let mut chars = input.chars();
        let current = chars.next();
        Self {
            input,
            chars,
            current_index: 0,
            next_index: current.map(|c| c.len_utf8()).unwrap_or(0),
            current,
        }
    }

    fn bump(&mut self) {
        if self.current.is_some() {
            self.current_index = self.next_index;
            self.next_index = self.current_index;
            if let Some(next) = self.chars.next() {
                self.next_index += next.len_utf8();
                self.current = Some(next);
            } else {
                self.current = None;
            }
        } else {
            self.current_index = self.input.len();
        }
    }

    fn peek(&self) -> Option<char> {
        self.current
    }

    fn peek_next(&self) -> Option<char> {
        let mut clone = self.chars.clone();
        clone.next()
    }

    fn lex(mut self) -> Result<Vec<Token>, Vec<Diagnostic>> {
        let mut tokens = Vec::new();
        let mut diagnostics = Vec::new();

        while let Some(ch) = self.peek() {
            let start = self.current_index;
            match ch {
                ' ' | '\t' | '\r' | '\n' => {
                    self.bump();
                }
                '/' => {
                    if self.peek_next() == Some('/') {
                        self.bump();
                        self.bump();
                        while let Some(c) = self.peek() {
                            if c == '\n' {
                                break;
                            }
                            self.bump();
                        }
                    } else if self.peek_next() == Some('*') {
                        self.bump();
                        self.bump();
                        let mut depth = 1;
                        while let Some(c) = self.peek() {
                            if c == '/' && self.peek_next() == Some('*') {
                                self.bump();
                                self.bump();
                                depth += 1;
                            } else if c == '*' && self.peek_next() == Some('/') {
                                self.bump();
                                self.bump();
                                depth -= 1;
                                if depth == 0 {
                                    break;
                                }
                            } else {
                                self.bump();
                            }
                        }
                        if depth > 0 {
                            diagnostics.push(
                                Diagnostic::new(
                                    DiagnosticKind::Lexer,
                                    "unterminated block comment",
                                )
                                .with_span(Span::new(start, self.input.len())),
                            );
                            break;
                        }
                    } else {
                        tokens.push(Token {
                            kind: TokenKind::Symbol(Symbol::Slash),
                            span: Span::new(start, start + 1),
                        });
                        self.bump();
                    }
                }
                '(' => {
                    tokens.push(self.simple(Symbol::LParen, start));
                    self.bump();
                }
                ')' => {
                    tokens.push(self.simple(Symbol::RParen, start));
                    self.bump();
                }
                '{' => {
                    tokens.push(self.simple(Symbol::LBrace, start));
                    self.bump();
                }
                '}' => {
                    tokens.push(self.simple(Symbol::RBrace, start));
                    self.bump();
                }
                '[' => {
                    tokens.push(self.simple(Symbol::LBracket, start));
                    self.bump();
                }
                ']' => {
                    tokens.push(self.simple(Symbol::RBracket, start));
                    self.bump();
                }
                ',' => {
                    tokens.push(self.simple(Symbol::Comma, start));
                    self.bump();
                }
                ':' => {
                    tokens.push(self.simple(Symbol::Colon, start));
                    self.bump();
                }
                ';' => {
                    tokens.push(self.simple(Symbol::Semicolon, start));
                    self.bump();
                }
                '.' => {
                    tokens.push(self.simple(Symbol::Dot, start));
                    self.bump();
                }
                '+' => {
                    tokens.push(self.simple(Symbol::Plus, start));
                    self.bump();
                }
                '-' => {
                    if self.peek_next() == Some('>') {
                        self.bump();
                        self.bump();
                        tokens.push(Token {
                            kind: TokenKind::Symbol(Symbol::Arrow),
                            span: Span::new(start, start + 2),
                        });
                    } else {
                        tokens.push(self.simple(Symbol::Minus, start));
                        self.bump();
                    }
                }
                '*' => {
                    tokens.push(self.simple(Symbol::Star, start));
                    self.bump();
                }
                '%' => {
                    tokens.push(self.simple(Symbol::Percent, start));
                    self.bump();
                }
                '=' => {
                    if self.peek_next() == Some('=') {
                        self.bump();
                        self.bump();
                        tokens.push(Token {
                            kind: TokenKind::Symbol(Symbol::DoubleEqual),
                            span: Span::new(start, start + 2),
                        });
                    } else if self.peek_next() == Some('>') {
                        self.bump();
                        self.bump();
                        tokens.push(Token {
                            kind: TokenKind::Symbol(Symbol::FatArrow),
                            span: Span::new(start, start + 2),
                        });
                    } else {
                        tokens.push(self.simple(Symbol::Equal, start));
                        self.bump();
                    }
                }
                '!' => {
                    if self.peek_next() == Some('=') {
                        self.bump();
                        self.bump();
                        tokens.push(Token {
                            kind: TokenKind::Symbol(Symbol::NotEqual),
                            span: Span::new(start, start + 2),
                        });
                    } else {
                        tokens.push(Token {
                            kind: TokenKind::Symbol(Symbol::Bang),
                            span: Span::new(start, start + 1),
                        });
                        self.bump();
                    }
                }
                '<' => {
                    if self.peek_next() == Some('=') {
                        self.bump();
                        self.bump();
                        tokens.push(Token {
                            kind: TokenKind::Symbol(Symbol::LessEqual),
                            span: Span::new(start, start + 2),
                        });
                    } else {
                        tokens.push(self.simple(Symbol::Less, start));
                        self.bump();
                    }
                }
                '>' => {
                    if self.peek_next() == Some('=') {
                        self.bump();
                        self.bump();
                        tokens.push(Token {
                            kind: TokenKind::Symbol(Symbol::GreaterEqual),
                            span: Span::new(start, start + 2),
                        });
                    } else {
                        tokens.push(self.simple(Symbol::Greater, start));
                        self.bump();
                    }
                }
                '&' => {
                    if self.peek_next() == Some('&') {
                        self.bump();
                        self.bump();
                        tokens.push(Token {
                            kind: TokenKind::Symbol(Symbol::And),
                            span: Span::new(start, start + 2),
                        });
                    } else {
                        diagnostics.push(
                            Diagnostic::new(DiagnosticKind::Lexer, "unexpected '&'")
                                .with_span(Span::new(start, start + 1)),
                        );
                        self.bump();
                    }
                }
                '|' => {
                    if self.peek_next() == Some('|') {
                        self.bump();
                        self.bump();
                        tokens.push(Token {
                            kind: TokenKind::Symbol(Symbol::Or),
                            span: Span::new(start, start + 2),
                        });
                    } else {
                        tokens.push(self.simple(Symbol::Pipe, start));
                        self.bump();
                    }
                }
                '"' => match self.lex_string(start) {
                    Ok(token) => tokens.push(token),
                    Err(diag) => diagnostics.push(diag),
                },
                ch if ch.is_ascii_digit() => match self.lex_number(start) {
                    Ok(token) => tokens.push(token),
                    Err(diag) => diagnostics.push(diag),
                },
                ch if is_start_ident(ch) => {
                    let token = self.lex_identifier(start);
                    tokens.push(token);
                }
                _ => {
                    diagnostics.push(
                        Diagnostic::new(
                            DiagnosticKind::Lexer,
                            format!("unexpected character '{}'", ch),
                        )
                        .with_span(Span::new(start, start + ch.len_utf8())),
                    );
                    self.bump();
                }
            }
        }

        tokens.push(Token {
            kind: TokenKind::Eof,
            span: Span::new(self.input.len(), self.input.len()),
        });

        if diagnostics.is_empty() {
            Ok(tokens)
        } else {
            Err(diagnostics)
        }
    }

    fn simple(&self, symbol: Symbol, start: usize) -> Token {
        Token {
            kind: TokenKind::Symbol(symbol),
            span: Span::new(start, start + 1),
        }
    }

    fn lex_string(&mut self, start: usize) -> Result<Token, Diagnostic> {
        self.bump(); // consume opening quote
        let mut value = String::new();
        while let Some(ch) = self.peek() {
            if ch == '"' {
                let end = self.next_index;
                self.bump();
                return Ok(Token {
                    kind: TokenKind::String(value),
                    span: Span::new(start, end),
                });
            } else if ch == '\\' {
                self.bump();
                if let Some(next) = self.peek() {
                    match next {
                        'n' => value.push('\n'),
                        'r' => value.push('\r'),
                        't' => value.push('\t'),
                        '\\' => value.push('\\'),
                        '"' => value.push('"'),
                        other => {
                            return Err(Diagnostic::new(
                                DiagnosticKind::Lexer,
                                format!("invalid escape sequence: \\{}", other),
                            )
                            .with_span(Span::new(start, self.next_index)));
                        }
                    }
                    self.bump();
                } else {
                    return Err(Diagnostic::new(
                        DiagnosticKind::Lexer,
                        "unterminated string literal",
                    )
                    .with_span(Span::new(start, self.input.len())));
                }
            } else {
                value.push(ch);
                self.bump();
            }
        }

        Err(
            Diagnostic::new(DiagnosticKind::Lexer, "unterminated string literal")
                .with_span(Span::new(start, self.input.len())),
        )
    }

    fn lex_number(&mut self, start: usize) -> Result<Token, Diagnostic> {
        let mut has_dot = false;
        let mut end = start;
        let mut value = String::new();

        while let Some(ch) = self.peek() {
            if ch == '_' {
                self.bump();
                continue;
            }

            if ch.is_ascii_digit() {
                end = self.next_index;
                value.push(ch);
                self.bump();
            } else if ch == '.' && !has_dot {
                has_dot = true;
                value.push('.');
                end = self.next_index;
                self.bump();
            } else {
                break;
            }
        }

        if has_dot {
            match value.parse::<f64>() {
                Ok(v) => Ok(Token {
                    kind: TokenKind::Float(v),
                    span: Span::new(start, end),
                }),
                Err(_) => Err(Diagnostic::new(
                    DiagnosticKind::Lexer,
                    "invalid floating point literal",
                )
                .with_span(Span::new(start, end))),
            }
        } else {
            match value.parse::<i64>() {
                Ok(v) => Ok(Token {
                    kind: TokenKind::Int(v),
                    span: Span::new(start, end),
                }),
                Err(_) => Err(
                    Diagnostic::new(DiagnosticKind::Lexer, "invalid integer literal")
                        .with_span(Span::new(start, end)),
                ),
            }
        }
    }

    fn lex_identifier(&mut self, start: usize) -> Token {
        let mut end = start;
        let mut ident = String::new();
        while let Some(ch) = self.peek() {
            if is_continue_ident(ch) {
                end = self.next_index;
                ident.push(ch);
                self.bump();
            } else {
                break;
            }
        }

        let kind = match ident.as_str() {
            "value" => TokenKind::Keyword(Keyword::Value),
            "struct" => TokenKind::Keyword(Keyword::Struct),
            "fn" => TokenKind::Keyword(Keyword::Fn),
            "source" => TokenKind::Keyword(Keyword::Source),
            "sink" => TokenKind::Keyword(Keyword::Sink),
            "normalize" => TokenKind::Keyword(Keyword::Normalize),
            "require" => TokenKind::Keyword(Keyword::Require),
            "always" => TokenKind::Keyword(Keyword::Always),
            "policy" => TokenKind::Keyword(Keyword::Policy),
            "invalid" => TokenKind::Keyword(Keyword::Invalid),
            "error" => TokenKind::Keyword(Keyword::Error),
            "drop" => TokenKind::Keyword(Keyword::Drop),
            "hold" => TokenKind::Keyword(Keyword::Hold),
            "replace" => TokenKind::Keyword(Keyword::Replace),
            "when" => TokenKind::Keyword(Keyword::When),
            "upon" => TokenKind::Keyword(Keyword::Upon),
            "next" => TokenKind::Keyword(Keyword::Next),
            "first" => TokenKind::Keyword(Keyword::First),
            "rest" => TokenKind::Keyword(Keyword::Rest),
            "defined" => TokenKind::Keyword(Keyword::Defined),
            "let" => TokenKind::Keyword(Keyword::Let),
            "in" => TokenKind::Keyword(Keyword::In),
            "if" => TokenKind::Keyword(Keyword::If),
            "else" => TokenKind::Keyword(Keyword::Else),
            "true" => TokenKind::Keyword(Keyword::True),
            "false" => TokenKind::Keyword(Keyword::False),
            "from" => TokenKind::Keyword(Keyword::From),
            "to" => TokenKind::Keyword(Keyword::To),
            _ => TokenKind::Identifier(ident),
        };

        Token {
            kind,
            span: Span::new(start, end),
        }
    }
}

fn is_start_ident(ch: char) -> bool {
    ch.is_ascii_alphabetic() || ch == '_'
}

fn is_continue_ident(ch: char) -> bool {
    is_start_ident(ch) || ch.is_ascii_digit()
}
