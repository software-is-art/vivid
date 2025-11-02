use crate::ast::*;
use crate::diagnostics::{Diagnostic, DiagnosticKind};
use crate::lexer::{Keyword, Symbol, Token, TokenKind, lex};
use crate::span::Span;

const ITEM_START: &[Keyword] = &[
    Keyword::Value,
    Keyword::Struct,
    Keyword::Fn,
    Keyword::Source,
    Keyword::Sink,
    Keyword::Let,
];

type ParseResult<T> = Result<T, ()>;

pub fn parse_module(source: &str) -> Result<Module, Vec<Diagnostic>> {
    let tokens = match lex(source) {
        Ok(tokens) => tokens,
        Err(diags) => return Err(diags),
    };
    let mut parser = Parser::new(source, tokens);
    parser.parse_module()
}

struct Parser<'a> {
    source: &'a str,
    tokens: Vec<Token>,
    position: usize,
    diagnostics: Vec<Diagnostic>,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str, tokens: Vec<Token>) -> Self {
        Self {
            source,
            tokens,
            position: 0,
            diagnostics: Vec::new(),
        }
    }

    fn parse_module(&mut self) -> Result<Module, Vec<Diagnostic>> {
        let mut items = Vec::new();

        while !self.is_at_end() {
            match self.parse_item() {
                Some(item) => items.push(item),
                None => self.synchronize(),
            }
        }

        if self.diagnostics.is_empty() {
            Ok(Module { items })
        } else {
            Err(std::mem::take(&mut self.diagnostics))
        }
    }

    fn parse_item(&mut self) -> Option<Item> {
        match self.peek_kind() {
            Some(TokenKind::Keyword(Keyword::Value)) => {
                self.advance();
                self.parse_value_decl().map(Item::Value).ok()
            }
            Some(TokenKind::Keyword(Keyword::Struct)) => {
                self.advance();
                self.parse_struct_decl().map(Item::Struct).ok()
            }
            Some(TokenKind::Keyword(Keyword::Fn)) => {
                self.advance();
                self.parse_function_decl().map(Item::Function).ok()
            }
            Some(TokenKind::Keyword(Keyword::Source)) => {
                self.advance();
                self.parse_source_decl().map(Item::Source).ok()
            }
            Some(TokenKind::Keyword(Keyword::Sink)) => {
                self.advance();
                self.parse_sink_decl().map(Item::Sink).ok()
            }
            Some(TokenKind::Keyword(Keyword::Let)) => {
                self.advance();
                self.parse_let_decl().map(Item::Let).ok()
            }
            Some(TokenKind::Eof) | None => None,
            _ => {
                let span = self.current_span();
                self.error(
                    span,
                    "unexpected token at top level; expected value, struct, fn, source, sink, or let",
                );
                self.advance();
                None
            }
        }
    }

    fn parse_value_decl(&mut self) -> ParseResult<ValueDecl> {
        let name = self.expect_ident("expected value name")?;
        let generics = self.parse_generics()?;
        self.expect_symbol(Symbol::Colon, "expected ':' after value name")?;
        let representation = self.parse_type_expr()?;
        let span_start = name.span.start;
        self.expect_symbol(Symbol::LBrace, "expected '{' to start value block")?;

        let mut normalize = Vec::new();
        let mut requires = Vec::new();
        let mut always = Vec::new();
        let mut policies = Vec::new();

        while !self.check_symbol(Symbol::RBrace) && !self.is_at_end() {
            match self.peek_kind() {
                Some(TokenKind::Keyword(Keyword::Normalize)) => {
                    let keyword_span = self.advance().span;
                    let rule = self.parse_normalize_rule(keyword_span)?;
                    normalize.push(rule);
                }
                Some(TokenKind::Keyword(Keyword::Require)) => {
                    self.advance();
                    let requirement = self.parse_expr()?;
                    self.consume_trailing_semicolon();
                    requires.push(requirement);
                }
                Some(TokenKind::Keyword(Keyword::Always)) => {
                    self.advance();
                    let guard = self.parse_expr()?;
                    self.consume_trailing_semicolon();
                    always.push(guard);
                }
                Some(TokenKind::Keyword(Keyword::Policy)) => {
                    let keyword_span = self.advance().span;
                    let policy = self.parse_policy_rule(keyword_span)?;
                    policies.push(policy);
                }
                _ => {
                    let span = self.current_span();
                    self.error(span, "unexpected item inside value block");
                    self.advance();
                }
            }
        }

        let rbrace_span = self
            .expect_symbol(Symbol::RBrace, "expected '}' to close value block")?
            .span;
        let span = Span::new(span_start, rbrace_span.end);

        Ok(ValueDecl {
            name,
            generics,
            representation,
            normalize,
            requires,
            always,
            policies,
            span,
        })
    }

    fn parse_normalize_rule(&mut self, keyword_span: Span) -> ParseResult<NormalizeRule> {
        let target = self.parse_pattern()?;
        self.expect_symbol(Symbol::Equal, "expected '=' after normalize pattern")?;
        let expr = self.parse_expr()?;
        self.consume_trailing_semicolon();
        let span = keyword_span.merge(expr.span());
        Ok(NormalizeRule { target, expr, span })
    }

    fn parse_policy_rule(&mut self, keyword_span: Span) -> ParseResult<PolicyRule> {
        let kind = if self.match_keyword(Keyword::Invalid) {
            PolicyKind::Invalid
        } else if self.match_keyword(Keyword::Error) {
            PolicyKind::Error
        } else {
            let ident = self.expect_ident("expected policy kind")?;
            PolicyKind::Custom(ident.name)
        };

        self.expect_symbol(Symbol::Equal, "expected '=' after policy kind")?;

        let (expr, expr_span) = if self.match_keyword(Keyword::Drop) {
            (
                PolicyExpr::Builtin(PolicyBuiltin::Drop),
                self.previous_span(),
            )
        } else if self.match_keyword(Keyword::Hold) {
            (
                PolicyExpr::Builtin(PolicyBuiltin::Hold),
                self.previous_span(),
            )
        } else if self.match_keyword(Keyword::Replace) {
            let replace_span = self.previous_span();
            let expr = self.parse_expr()?;
            let span = replace_span.merge(expr.span());
            (PolicyExpr::Replace(Box::new(expr)), span)
        } else {
            let expr = self.parse_expr()?;
            let span = expr.span();
            (PolicyExpr::Expr(expr), span)
        };
        self.consume_trailing_semicolon();
        let span = keyword_span.merge(expr_span);
        Ok(PolicyRule { kind, expr, span })
    }

    fn parse_struct_decl(&mut self) -> ParseResult<StructDecl> {
        let name = self.expect_ident("expected struct name")?;
        let span_start = name.span.start;
        self.expect_symbol(Symbol::LBrace, "expected '{' after struct name")?;
        let mut fields = Vec::new();
        while !self.check_symbol(Symbol::RBrace) && !self.is_at_end() {
            let field_name = self.expect_ident("expected field name")?;
            self.expect_symbol(Symbol::Colon, "expected ':' after field name")?;
            let ty = self.parse_type_expr()?;
            fields.push(StructField {
                name: field_name,
                ty,
            });
            if !self.match_symbol(Symbol::Comma) {
                break;
            }
        }
        let rbrace_span = self
            .expect_symbol(Symbol::RBrace, "expected '}' to close struct")?
            .span;
        let span = Span::new(span_start, rbrace_span.end);
        Ok(StructDecl { name, fields, span })
    }

    fn parse_function_decl(&mut self) -> ParseResult<FunctionDecl> {
        let name = self.expect_ident("expected function name")?;
        let span_start = name.span.start;
        self.expect_symbol(Symbol::LParen, "expected '(' after function name")?;
        let mut params = Vec::new();
        if !self.check_symbol(Symbol::RParen) {
            loop {
                let pattern = self.parse_pattern()?;
                params.push(pattern);
                if !self.match_symbol(Symbol::Comma) {
                    break;
                }
            }
        }
        self.expect_symbol(Symbol::RParen, "expected ')' to close parameter list")?;

        let return_type = if self.match_symbol(Symbol::Arrow) {
            Some(self.parse_type_expr()?)
        } else {
            None
        };

        let body = if self.match_symbol(Symbol::Equal) {
            let expr = self.parse_expr()?;
            self.consume_trailing_semicolon();
            expr
        } else {
            self.parse_block_expr()?
        };

        let span = Span::new(span_start, body.span().end);

        Ok(FunctionDecl {
            name,
            params,
            return_type,
            body,
            span,
        })
    }

    fn parse_source_decl(&mut self) -> ParseResult<SourceDecl> {
        let name = self.expect_ident("expected source name")?;
        let span_start = name.span.start;
        self.expect_symbol(Symbol::Colon, "expected ':' after source name")?;
        let ty = self.parse_type_expr()?;
        self.expect_keyword(Keyword::From, "expected 'from' in source declaration")?;
        let provider = self.parse_expr()?;
        self.consume_trailing_semicolon();
        let span = Span::new(span_start, provider.span().end);
        Ok(SourceDecl {
            name,
            ty,
            provider,
            span,
        })
    }

    fn parse_sink_decl(&mut self) -> ParseResult<SinkDecl> {
        let name = self.expect_ident("expected sink name")?;
        let span_start = name.span.start;
        self.expect_symbol(Symbol::Colon, "expected ':' after sink name")?;
        let ty = self.parse_type_expr()?;
        self.expect_keyword(Keyword::To, "expected 'to' in sink declaration")?;
        let target = self.parse_expr()?;
        self.consume_trailing_semicolon();
        let span = Span::new(span_start, target.span().end);
        Ok(SinkDecl {
            name,
            ty,
            target,
            span,
        })
    }

    fn parse_let_decl(&mut self) -> ParseResult<LetDecl> {
        let pattern = self.parse_pattern()?;
        let span_start = pattern.span().start;
        self.expect_symbol(Symbol::Equal, "expected '=' in let binding")?;
        let expr = self.parse_expr()?;
        self.consume_trailing_semicolon();
        let span = Span::new(span_start, expr.span().end);
        Ok(LetDecl {
            pattern,
            expr,
            span,
        })
    }

    fn parse_generics(&mut self) -> ParseResult<Vec<Ident>> {
        if !self.match_symbol(Symbol::Less) {
            return Ok(Vec::new());
        }
        let mut params = Vec::new();
        loop {
            let ident = self.expect_ident("expected generic parameter")?;
            params.push(ident);
            if self.match_symbol(Symbol::Comma) {
                continue;
            }
            self.expect_symbol(Symbol::Greater, "expected '>' to close generics")?;
            break;
        }
        Ok(params)
    }

    fn parse_type_expr(&mut self) -> ParseResult<TypeExpr> {
        let start = self.current_span().start;
        match self.peek_kind() {
            Some(TokenKind::Identifier(_)) => {
                let ident = self.expect_ident("expected type name")?;
                let mut end = ident.span.end;
                let mut kind = TypeExprKind::Named(ident.name.clone());

                if self.check_symbol(Symbol::Less) {
                    self.advance();
                    let mut args = Vec::new();
                    if !self.check_symbol(Symbol::Greater) {
                        loop {
                            let arg = self.parse_type_expr()?;
                            args.push(arg);
                            if self.match_symbol(Symbol::Comma) {
                                continue;
                            }
                            break;
                        }
                    }
                    let gt = self
                        .expect_symbol(Symbol::Greater, "expected '>' to close type arguments")?;
                    end = gt.span.end;
                    if ident.name == "S" && args.len() == 1 {
                        let arg = args.pop().unwrap();
                        kind = TypeExprKind::Stream(Box::new(arg));
                    } else {
                        kind = TypeExprKind::Applied {
                            base: ident.name.clone(),
                            args,
                        };
                    }
                }

                Ok(TypeExpr {
                    kind,
                    span: Span::new(start, end),
                })
            }
            Some(TokenKind::Symbol(Symbol::LParen)) => {
                let lparen = self.advance();
                let mut elements = Vec::new();
                if !self.check_symbol(Symbol::RParen) {
                    loop {
                        let elem = self.parse_type_expr()?;
                        elements.push(elem);
                        if !self.match_symbol(Symbol::Comma) {
                            break;
                        }
                    }
                }
                let closing =
                    self.expect_symbol(Symbol::RParen, "expected ')' to close tuple type")?;
                let span_end = closing.span.end;
                Ok(TypeExpr {
                    kind: TypeExprKind::Tuple(elements),
                    span: Span::new(lparen.span.start, span_end),
                })
            }
            _ => {
                let span = self.current_span();
                self.error(span, "expected type expression");
                Err(())
            }
        }
    }

    fn parse_pattern(&mut self) -> ParseResult<Pattern> {
        match self.peek_kind() {
            Some(TokenKind::Identifier(_)) => {
                let ident = self.expect_ident("expected identifier")?;
                if ident.name == "_" {
                    Ok(Pattern::Wildcard(ident.span))
                } else {
                    Ok(Pattern::Identifier(ident))
                }
            }
            Some(TokenKind::Symbol(Symbol::LParen)) => {
                let start = self.advance().span;
                let mut elements = Vec::new();
                if !self.check_symbol(Symbol::RParen) {
                    loop {
                        elements.push(self.parse_pattern()?);
                        if !self.match_symbol(Symbol::Comma) {
                            break;
                        }
                    }
                }
                let end = self
                    .expect_symbol(Symbol::RParen, "expected ')' in tuple pattern")?
                    .span;
                Ok(Pattern::Tuple(elements, Span::new(start.start, end.end)))
            }
            _ => {
                let span = self.current_span();
                self.error(span, "expected pattern");
                Err(())
            }
        }
    }

    fn parse_expr(&mut self) -> ParseResult<Expr> {
        self.parse_fby_expr()
    }

    fn parse_fby_expr(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_when_expr()?;
        while self.match_identifier("fby") {
            let rhs = self.parse_when_expr()?;
            let span = expr.span().merge(rhs.span());
            expr = Expr::Binary {
                op: BinaryOp::Fby,
                left: Box::new(expr),
                right: Box::new(rhs),
                span,
            };
        }
        Ok(expr)
    }

    fn parse_when_expr(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_logical_or()?;
        loop {
            if self.match_keyword(Keyword::When) {
                let keyword_span = self.previous_span();
                let rhs = self.parse_logical_or()?;
                let rhs_span = rhs.span();
                let left_span = expr.span();
                let span = left_span.merge(rhs_span);
                let left_expr = expr;
                expr = Expr::Call {
                    callee: Box::new(Expr::Identifier(Ident::new("when", keyword_span))),
                    arguments: vec![
                        Argument {
                            label: None,
                            expr: left_expr,
                            span: left_span,
                        },
                        Argument {
                            label: None,
                            expr: rhs,
                            span: rhs_span,
                        },
                    ],
                    span,
                };
            } else if self.match_keyword(Keyword::Upon) {
                let keyword_span = self.previous_span();
                let rhs = self.parse_logical_or()?;
                let rhs_span = rhs.span();
                let left_span = expr.span();
                let span = left_span.merge(rhs_span);
                let left_expr = expr;
                expr = Expr::Call {
                    callee: Box::new(Expr::Identifier(Ident::new("upon", keyword_span))),
                    arguments: vec![
                        Argument {
                            label: None,
                            expr: left_expr,
                            span: left_span,
                        },
                        Argument {
                            label: None,
                            expr: rhs,
                            span: rhs_span,
                        },
                    ],
                    span,
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_logical_or(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_logical_and()?;
        while self.match_symbol(Symbol::Or) {
            let rhs = self.parse_logical_and()?;
            let span = expr.span().merge(rhs.span());
            expr = Expr::Binary {
                op: BinaryOp::Or,
                left: Box::new(expr),
                right: Box::new(rhs),
                span,
            };
        }
        Ok(expr)
    }

    fn parse_logical_and(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_equality()?;
        while self.match_symbol(Symbol::And) {
            let rhs = self.parse_equality()?;
            let span = expr.span().merge(rhs.span());
            expr = Expr::Binary {
                op: BinaryOp::And,
                left: Box::new(expr),
                right: Box::new(rhs),
                span,
            };
        }
        Ok(expr)
    }

    fn parse_equality(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_comparison()?;
        loop {
            if self.match_symbol(Symbol::DoubleEqual) {
                let rhs = self.parse_comparison()?;
                let span = expr.span().merge(rhs.span());
                expr = Expr::Binary {
                    op: BinaryOp::Eq,
                    left: Box::new(expr),
                    right: Box::new(rhs),
                    span,
                };
            } else if self.match_symbol(Symbol::NotEqual) {
                let rhs = self.parse_comparison()?;
                let span = expr.span().merge(rhs.span());
                expr = Expr::Binary {
                    op: BinaryOp::Neq,
                    left: Box::new(expr),
                    right: Box::new(rhs),
                    span,
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_comparison(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_term()?;
        loop {
            if self.match_symbol(Symbol::Less) {
                let rhs = self.parse_term()?;
                let span = expr.span().merge(rhs.span());
                expr = Expr::Binary {
                    op: BinaryOp::Lt,
                    left: Box::new(expr),
                    right: Box::new(rhs),
                    span,
                };
            } else if self.match_symbol(Symbol::LessEqual) {
                let rhs = self.parse_term()?;
                let span = expr.span().merge(rhs.span());
                expr = Expr::Binary {
                    op: BinaryOp::Lte,
                    left: Box::new(expr),
                    right: Box::new(rhs),
                    span,
                };
            } else if self.match_symbol(Symbol::Greater) {
                let rhs = self.parse_term()?;
                let span = expr.span().merge(rhs.span());
                expr = Expr::Binary {
                    op: BinaryOp::Gt,
                    left: Box::new(expr),
                    right: Box::new(rhs),
                    span,
                };
            } else if self.match_symbol(Symbol::GreaterEqual) {
                let rhs = self.parse_term()?;
                let span = expr.span().merge(rhs.span());
                expr = Expr::Binary {
                    op: BinaryOp::Gte,
                    left: Box::new(expr),
                    right: Box::new(rhs),
                    span,
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_term(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_factor()?;
        loop {
            if self.match_symbol(Symbol::Plus) {
                let rhs = self.parse_factor()?;
                let span = expr.span().merge(rhs.span());
                expr = Expr::Binary {
                    op: BinaryOp::Add,
                    left: Box::new(expr),
                    right: Box::new(rhs),
                    span,
                };
            } else if self.match_symbol(Symbol::Minus) {
                let rhs = self.parse_factor()?;
                let span = expr.span().merge(rhs.span());
                expr = Expr::Binary {
                    op: BinaryOp::Sub,
                    left: Box::new(expr),
                    right: Box::new(rhs),
                    span,
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_factor(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_unary()?;
        loop {
            if self.match_symbol(Symbol::Star) {
                let rhs = self.parse_unary()?;
                let span = expr.span().merge(rhs.span());
                expr = Expr::Binary {
                    op: BinaryOp::Mul,
                    left: Box::new(expr),
                    right: Box::new(rhs),
                    span,
                };
            } else if self.match_symbol(Symbol::Slash) {
                let rhs = self.parse_unary()?;
                let span = expr.span().merge(rhs.span());
                expr = Expr::Binary {
                    op: BinaryOp::Div,
                    left: Box::new(expr),
                    right: Box::new(rhs),
                    span,
                };
            } else if self.match_symbol(Symbol::Percent) {
                let rhs = self.parse_unary()?;
                let span = expr.span().merge(rhs.span());
                expr = Expr::Binary {
                    op: BinaryOp::Mod,
                    left: Box::new(expr),
                    right: Box::new(rhs),
                    span,
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_unary(&mut self) -> ParseResult<Expr> {
        if self.match_symbol(Symbol::Minus) {
            let operand = self.parse_unary()?;
            let span = Span::new(self.previous_span().start, operand.span().end);
            return Ok(Expr::Unary {
                op: UnaryOp::Neg,
                expr: Box::new(operand),
                span,
            });
        }
        if self.match_symbol(Symbol::Bang) {
            let operand = self.parse_unary()?;
            let span = Span::new(self.previous_span().start, operand.span().end);
            return Ok(Expr::Unary {
                op: UnaryOp::Not,
                expr: Box::new(operand),
                span,
            });
        }
        if self.match_keyword(Keyword::Next) {
            let keyword_span = self.previous_span();
            let operand = self.parse_unary()?;
            return Ok(self.make_unary_call("next", keyword_span, operand));
        }
        if self.match_keyword(Keyword::First) {
            let keyword_span = self.previous_span();
            let operand = self.parse_unary()?;
            return Ok(self.make_unary_call("first", keyword_span, operand));
        }
        if self.match_keyword(Keyword::Rest) {
            let keyword_span = self.previous_span();
            let operand = self.parse_unary()?;
            return Ok(self.make_unary_call("rest", keyword_span, operand));
        }
        if self.match_keyword(Keyword::Defined) {
            let keyword_span = self.previous_span();
            let operand = self.parse_unary()?;
            return Ok(self.make_unary_call("defined", keyword_span, operand));
        }
        if self.match_keyword(Keyword::Hold) {
            let keyword_span = self.previous_span();
            let operand = self.parse_unary()?;
            return Ok(self.make_unary_call("hold", keyword_span, operand));
        }
        self.parse_call()
    }

    fn parse_call(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_primary()?;

        loop {
            if self.match_symbol(Symbol::LParen) {
                let mut args = Vec::new();
                if !self.check_symbol(Symbol::RParen) {
                    loop {
                        let arg_start = self.current_span().start;
                        let label = if matches!(self.peek_kind(), Some(TokenKind::Identifier(_)))
                            && self.peek_next_is_symbol(Symbol::Colon)
                        {
                            let ident = self.expect_ident("expected argument label")?;
                            self.expect_symbol(Symbol::Colon, "expected ':' after argument label")?;
                            Some(ident)
                        } else {
                            None
                        };
                        let expr_arg = self.parse_expr()?;
                        let span = Span::new(arg_start, expr_arg.span().end);
                        args.push(Argument {
                            label,
                            expr: expr_arg,
                            span,
                        });
                        if !self.match_symbol(Symbol::Comma) {
                            break;
                        }
                    }
                }
                let closing =
                    self.expect_symbol(Symbol::RParen, "expected ')' to close arguments")?;
                let span = expr.span().merge(closing.span);
                expr = Expr::Call {
                    callee: Box::new(expr),
                    arguments: args,
                    span,
                };
            } else if self.match_symbol(Symbol::Dot) {
                let field = self.expect_ident("expected field name after '.'")?;
                let span = expr.span().merge(field.span);
                expr = Expr::Access {
                    target: Box::new(expr),
                    field,
                    span,
                };
            } else if self.match_symbol(Symbol::LBracket) {
                let index = self.parse_expr()?;
                let closing =
                    self.expect_symbol(Symbol::RBracket, "expected ']' to close index expression")?;
                let span = expr.span().merge(closing.span);
                expr = Expr::Index {
                    target: Box::new(expr),
                    index: Box::new(index),
                    span,
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> ParseResult<Expr> {
        match self.peek_kind().cloned() {
            Some(TokenKind::Int(value)) => {
                let token = self.advance();
                Ok(Expr::Literal(Literal::Int(value), token.span))
            }
            Some(TokenKind::Float(value)) => {
                let token = self.advance();
                Ok(Expr::Literal(Literal::Float(value), token.span))
            }
            Some(TokenKind::Keyword(Keyword::True)) => {
                let token = self.advance();
                Ok(Expr::Literal(Literal::Bool(true), token.span))
            }
            Some(TokenKind::Keyword(Keyword::False)) => {
                let token = self.advance();
                Ok(Expr::Literal(Literal::Bool(false), token.span))
            }
            Some(TokenKind::String(value)) => {
                let token = self.advance();
                Ok(Expr::Literal(Literal::String(value), token.span))
            }
            Some(TokenKind::Identifier(_)) => {
                let ident = self.expect_ident("expected identifier")?;
                Ok(Expr::Identifier(ident))
            }
            Some(TokenKind::Keyword(Keyword::If)) => {
                self.advance();
                self.parse_if_expr()
            }
            Some(TokenKind::Keyword(Keyword::Let)) => {
                self.advance();
                self.parse_let_expr()
            }
            Some(TokenKind::Symbol(Symbol::LParen)) => {
                let start = self.advance().span;
                if self.check_symbol(Symbol::RParen) {
                    let end = self.advance().span;
                    Ok(Expr::Tuple(Vec::new(), Span::new(start.start, end.end)))
                } else {
                    let first = self.parse_expr()?;
                    if self.match_symbol(Symbol::Comma) {
                        let mut elements = vec![first];
                        while !self.check_symbol(Symbol::RParen) {
                            elements.push(self.parse_expr()?);
                            if !self.match_symbol(Symbol::Comma) {
                                break;
                            }
                        }
                        let end = self
                            .expect_symbol(Symbol::RParen, "expected ')' to close tuple")?
                            .span;
                        Ok(Expr::Tuple(elements, Span::new(start.start, end.end)))
                    } else {
                        self.expect_symbol(Symbol::RParen, "expected ')' after expression")?;
                        Ok(first)
                    }
                }
            }
            Some(TokenKind::Symbol(Symbol::LBracket)) => self.parse_list_expr(),
            Some(TokenKind::Symbol(Symbol::LBrace)) => {
                if self.peek_record_literal() {
                    self.parse_record_expr()
                } else {
                    self.parse_block_expr()
                }
            }
            _ => {
                let span = self.current_span();
                self.error(span, "unexpected token in expression");
                Err(())
            }
        }
    }

    fn parse_record_expr(&mut self) -> ParseResult<Expr> {
        let start = self.expect_symbol(Symbol::LBrace, "expected '{'")?.span;
        let mut fields = Vec::new();
        if !self.check_symbol(Symbol::RBrace) {
            loop {
                let ident = self.expect_ident("expected field name in record literal")?;
                self.expect_symbol(Symbol::Colon, "expected ':' after field name")?;
                let expr = self.parse_expr()?;
                fields.push((ident, expr));
                if !self.match_symbol(Symbol::Comma) {
                    break;
                }
            }
        }
        let end = self
            .expect_symbol(Symbol::RBrace, "expected '}' to close record literal")?
            .span;
        Ok(Expr::Record(fields, Span::new(start.start, end.end)))
    }

    fn parse_if_expr(&mut self) -> ParseResult<Expr> {
        let if_span = self.previous_span();
        let condition = self.parse_expr()?;
        let then_branch = self.parse_block_expr()?;
        let then_span = then_branch.span();
        let else_expr = if self.match_keyword(Keyword::Else) {
            Some(if self.check_symbol(Symbol::LBrace) {
                self.parse_block_expr()?
            } else {
                self.parse_expr()?
            })
        } else {
            None
        };
        let span_end = if let Some(ref else_expr) = else_expr {
            then_span.merge(else_expr.span()).end
        } else {
            then_span.end
        };
        Ok(Expr::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: else_expr.map(Box::new),
            span: Span::new(if_span.start, span_end),
        })
    }

    fn parse_let_expr(&mut self) -> ParseResult<Expr> {
        let keyword_span = self.previous_span();
        let mut bindings = Vec::new();
        loop {
            let pattern = self.parse_pattern()?;
            let pattern_span = pattern.span();
            self.expect_symbol(Symbol::Equal, "expected '=' in let expression")?;
            let expr = self.parse_expr()?;
            let binding_span = pattern_span.merge(expr.span());
            bindings.push(LetBinding {
                pattern,
                expr,
                span: binding_span,
            });
            if !self.match_symbol(Symbol::Comma) {
                break;
            }
        }
        self.expect_keyword(Keyword::In, "expected 'in' after let bindings")?;
        let body = self.parse_expr()?;
        let span = keyword_span.merge(body.span());
        Ok(Expr::Let {
            bindings,
            body: Box::new(body),
            span,
        })
    }

    fn parse_list_expr(&mut self) -> ParseResult<Expr> {
        let start = self.expect_symbol(Symbol::LBracket, "expected '['")?.span;
        let mut elements = Vec::new();
        if !self.check_symbol(Symbol::RBracket) {
            loop {
                elements.push(self.parse_expr()?);
                if !self.match_symbol(Symbol::Comma) {
                    break;
                }
            }
        }
        let end = self
            .expect_symbol(Symbol::RBracket, "expected ']' to close list")?
            .span;
        Ok(Expr::List(elements, Span::new(start.start, end.end)))
    }

    fn parse_block_expr(&mut self) -> ParseResult<Expr> {
        let start = self.expect_symbol(Symbol::LBrace, "expected '{'")?.span;
        let mut statements = Vec::new();
        let mut tail = None;
        while !self.check_symbol(Symbol::RBrace) && !self.is_at_end() {
            if self.match_keyword(Keyword::Let) {
                let let_span = self.previous_span();
                let pattern = self.parse_pattern()?;
                self.expect_symbol(Symbol::Equal, "expected '=' in let statement")?;
                let expr = self.parse_expr()?;
                let expr_span = expr.span();
                let binding_span = let_span.merge(expr_span);
                self.consume_trailing_semicolon();
                statements.push(Stmt::Let(LetBinding {
                    pattern,
                    expr,
                    span: binding_span,
                }));
            } else {
                let expr = self.parse_expr()?;
                if self.match_symbol(Symbol::Semicolon) {
                    statements.push(Stmt::Expr(expr));
                } else {
                    tail = Some(Box::new(expr));
                    break;
                }
            }
        }
        let end = self
            .expect_symbol(Symbol::RBrace, "expected '}' to close block")?
            .span;
        Ok(Expr::Block {
            statements,
            tail,
            span: Span::new(start.start, end.end),
        })
    }

    fn consume_trailing_semicolon(&mut self) {
        let _ = self.match_symbol(Symbol::Semicolon);
    }

    fn make_unary_call(&self, name: &str, keyword_span: Span, operand: Expr) -> Expr {
        let operand_span = operand.span();
        let span = keyword_span.merge(operand_span);
        Expr::Call {
            callee: Box::new(Expr::Identifier(Ident::new(name.to_string(), keyword_span))),
            arguments: vec![Argument {
                label: None,
                expr: operand,
                span: operand_span,
            }],
            span,
        }
    }

    fn match_keyword(&mut self, keyword: Keyword) -> bool {
        if matches!(self.peek_kind(), Some(TokenKind::Keyword(k)) if *k == keyword) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect_keyword(&mut self, keyword: Keyword, message: &str) -> ParseResult<Token> {
        if matches!(self.peek_kind(), Some(TokenKind::Keyword(k)) if *k == keyword) {
            Ok(self.advance())
        } else {
            let span = self.current_span();
            self.error(span, message);
            Err(())
        }
    }

    fn match_symbol(&mut self, symbol: Symbol) -> bool {
        if matches!(self.peek_kind(), Some(TokenKind::Symbol(s)) if *s == symbol) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn check_symbol(&self, symbol: Symbol) -> bool {
        matches!(self.peek_kind(), Some(TokenKind::Symbol(s)) if *s == symbol)
    }

    fn expect_symbol(&mut self, symbol: Symbol, message: &str) -> ParseResult<Token> {
        if matches!(self.peek_kind(), Some(TokenKind::Symbol(s)) if *s == symbol) {
            Ok(self.advance())
        } else {
            let span = self.current_span();
            self.error(span, message);
            Err(())
        }
    }

    fn expect_ident(&mut self, message: &str) -> ParseResult<Ident> {
        if matches!(self.peek_kind(), Some(TokenKind::Identifier(_))) {
            let token = self.advance();
            if let TokenKind::Identifier(name) = token.kind {
                Ok(Ident::new(name, token.span))
            } else {
                unreachable!("token kind changed during expect_ident")
            }
        } else {
            let span = self.current_span();
            self.error(span, message);
            Err(())
        }
    }

    fn match_identifier(&mut self, expected: &str) -> bool {
        match self.peek_kind() {
            Some(TokenKind::Identifier(name)) if name == expected => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    fn peek_record_literal(&self) -> bool {
        match self.tokens.get(self.position + 1) {
            Some(Token {
                kind: TokenKind::Symbol(Symbol::RBrace),
                ..
            }) => true,
            Some(Token {
                kind: TokenKind::Identifier(_),
                ..
            }) => matches!(
                self.tokens.get(self.position + 2),
                Some(Token {
                    kind: TokenKind::Symbol(Symbol::Colon),
                    ..
                })
            ),
            _ => false,
        }
    }

    fn peek_next_is_symbol(&self, symbol: Symbol) -> bool {
        match self.tokens.get(self.position + 1) {
            Some(Token {
                kind: TokenKind::Symbol(s),
                ..
            }) if *s == symbol => true,
            _ => false,
        }
    }

    fn is_at_end(&self) -> bool {
        matches!(self.peek_kind(), Some(TokenKind::Eof))
    }

    fn peek_kind(&self) -> Option<&TokenKind> {
        self.tokens.get(self.position).map(|token| &token.kind)
    }

    fn advance(&mut self) -> Token {
        let token = self.tokens[self.position].clone();
        if !matches!(token.kind, TokenKind::Eof) {
            self.position += 1;
        }
        token
    }

    fn previous_span(&self) -> Span {
        if self.position == 0 {
            Span::new(0, 0)
        } else {
            self.tokens[self.position - 1].span
        }
    }

    fn current_span(&self) -> Span {
        self.tokens
            .get(self.position)
            .map(|token| token.span)
            .unwrap_or_else(|| Span::new(self.source.len(), self.source.len()))
    }

    fn error(&mut self, span: Span, message: impl Into<String>) {
        self.diagnostics
            .push(Diagnostic::new(DiagnosticKind::Parser, message).with_span(span));
    }

    fn synchronize(&mut self) {
        while !self.is_at_end() {
            if matches!(self.peek_kind(), Some(TokenKind::Symbol(Symbol::Semicolon))) {
                self.advance();
                return;
            }

            if matches!(self.peek_kind(), Some(TokenKind::Keyword(k)) if ITEM_START.contains(k)) {
                return;
            }

            self.advance();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_ok(source: &str) -> Module {
        parse_module(source).expect("parser returned diagnostics")
    }

    #[test]
    fn parses_basic_value_block() {
        let module = parse_ok(
            r#"
            value Email : String {
                normalize it = it.trim().toLowerAscii()
                require it.contains("@")
                always unique(it)
                policy invalid = drop
                policy error = InvalidEmail(it)
            }
            "#,
        );
        assert_eq!(module.items.len(), 1);
        match &module.items[0] {
            Item::Value(value) => {
                assert_eq!(value.name.name, "Email");
                assert_eq!(value.normalize.len(), 1);
                assert_eq!(value.requires.len(), 1);
                assert_eq!(value.always.len(), 1);
                assert_eq!(value.policies.len(), 2);
            }
            other => panic!("expected value item, found {:?}", other),
        }
    }

    #[test]
    fn parses_function_with_block_body() {
        let module = parse_ok(
            r#"
            fn increment(x) {
                let y = x + 1;
                y
            }
            "#,
        );
        assert_eq!(module.items.len(), 1);
        match &module.items[0] {
            Item::Function(func) => {
                assert_eq!(func.name.name, "increment");
                assert_eq!(func.params.len(), 1);
            }
            other => panic!("expected function item, found {:?}", other),
        }
    }
}
