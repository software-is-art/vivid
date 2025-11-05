use std::io::{self, Write};

use vivid::diagnostics::{Diagnostic, DiagnosticKind};
use vivid::parser::parse_module;
use vivid::runtime::Interpreter;
use vivid::runtime::stream::Stream;
use vivid::span::Span;

const PROMPT_PRIMARY: &str = "vivid> ";
const PROMPT_CONTINUATION: &str = "....> ";
const TEMP_BINDING: &str = "__it";
const PREVIEW_TICKS: usize = 8;

fn main() {
    if let Err(err) = run_repl() {
        eprintln!("io error: {}", err);
        std::process::exit(1);
    }
}

fn run_repl() -> io::Result<()> {
    let mut interpreter = Interpreter::new();
    println!("Vivid REPL — type :help for commands, :quit to exit.");

    loop {
        match read_snippet(&mut interpreter)? {
            Input::Eof => return Ok(()),
            Input::Empty | Input::Command => continue,
            Input::Code {
                source,
                is_definition,
            } => {
                process_snippet(&source, is_definition, &mut interpreter);
            }
        }
    }
}

enum Input {
    Eof,
    Empty,
    Command,
    Code { source: String, is_definition: bool },
}

fn read_snippet(interpreter: &mut Interpreter) -> io::Result<Input> {
    let mut snippet = String::new();
    let mut buffer = String::new();

    loop {
        let prompt = if snippet.is_empty() {
            PROMPT_PRIMARY
        } else {
            PROMPT_CONTINUATION
        };
        print!("{}", prompt);
        io::stdout().flush()?;

        buffer.clear();
        let bytes = io::stdin().read_line(&mut buffer)?;
        if bytes == 0 {
            if snippet.trim().is_empty() {
                println!();
                return Ok(Input::Eof);
            } else {
                break;
            }
        }

        let line = buffer.trim_end_matches('\n');

        if snippet.is_empty() {
            let trimmed = line.trim();
            if trimmed.starts_with(':') {
                if !handle_command(trimmed, interpreter)? {
                    return Ok(Input::Eof);
                }
                return Ok(Input::Command);
            }
        }

        if line.trim().is_empty() {
            if snippet.trim().is_empty() {
                continue;
            } else {
                break;
            }
        }

        snippet.push_str(line);
        snippet.push('\n');
    }

    let trimmed = snippet.trim();
    if trimmed.is_empty() {
        return Ok(Input::Empty);
    }
    let is_definition = looks_like_definition(trimmed);
    Ok(Input::Code {
        source: snippet,
        is_definition,
    })
}

fn handle_command(command: &str, interpreter: &mut Interpreter) -> io::Result<bool> {
    let remainder = command.trim_start_matches(':').trim();
    if remainder.is_empty() {
        print_help();
        return Ok(true);
    }

    let mut parts = remainder.split_whitespace();
    let cmd = match parts.next() {
        Some(cmd) => cmd,
        None => {
            print_help();
            return Ok(true);
        }
    };

    match cmd {
        "quit" | "exit" => Ok(false),
        "help" => {
            print_help();
            Ok(true)
        }
        "first" => {
            if let Some(name) = parts.next() {
                if parts.next().is_some() {
                    println!("usage: :first <name>");
                    return Ok(true);
                }
                if let Some(stream) = resolve_stream(interpreter, name) {
                    print_single_value(&stream, 0);
                } else {
                    println!("unknown stream '{}'", name);
                }
            } else {
                println!("usage: :first <name>");
            }
            Ok(true)
        }
        "at" => {
            let name = if let Some(name) = parts.next() {
                name
            } else {
                println!("usage: :at <name> <tick>");
                return Ok(true);
            };
            let tick_str = if let Some(tick) = parts.next() {
                tick
            } else {
                println!("usage: :at <name> <tick>");
                return Ok(true);
            };
            if parts.next().is_some() {
                println!("usage: :at <name> <tick>");
                return Ok(true);
            }
            match tick_str.parse::<usize>() {
                Ok(tick) => {
                    if let Some(stream) = resolve_stream(interpreter, name) {
                        print_single_value(&stream, tick);
                    } else {
                        println!("unknown stream '{}'", name);
                    }
                }
                Err(_) => println!("invalid tick '{}'; expected non-negative integer", tick_str),
            }
            Ok(true)
        }
        "rest" => {
            if let Some(name) = parts.next() {
                let ticks = parts
                    .next()
                    .map(|value| value.parse::<usize>().unwrap_or(PREVIEW_TICKS))
                    .unwrap_or(PREVIEW_TICKS);
                if parts.next().is_some() {
                    println!("usage: :rest <name> [ticks]");
                    return Ok(true);
                }
                match derive_stream(interpreter, &format!("rest({})", name)) {
                    Ok(stream) => print_stream_preview(&stream, ticks),
                    Err(diags) => print_diagnostics(&diags, ""),
                }
            } else {
                println!("usage: :rest <name> [ticks]");
            }
            Ok(true)
        }
        "show" => {
            if let Some(name) = parts.next() {
                let ticks = parts
                    .next()
                    .map(|value| value.parse::<usize>().unwrap_or(PREVIEW_TICKS))
                    .unwrap_or(PREVIEW_TICKS);
                if parts.next().is_some() {
                    println!("usage: :show <name> [ticks]");
                    return Ok(true);
                }
                if let Some(stream) = resolve_stream(interpreter, name) {
                    print_stream_preview(&stream, ticks);
                } else {
                    println!("unknown stream '{}'", name);
                }
            } else {
                println!("usage: :show <name> [ticks]");
            }
            Ok(true)
        }
        other => {
            println!("unknown command '{}'", other);
            print_help();
            Ok(true)
        }
    }
}

fn process_snippet(source: &str, is_definition: bool, interpreter: &mut Interpreter) {
    if is_definition {
        match parse_module(source) {
            Ok(module) => match interpreter.evaluate_module(&module) {
                Ok(()) => println!("ok"),
                Err(diags) => {
                    print_diagnostics(&diags, source);
                }
            },
            Err(diags) => {
                print_diagnostics(&diags, source);
            }
        }
    } else {
        let expr = source.trim();
        let module_source = format!("let {} = ({});", TEMP_BINDING, expr);
        match parse_module(&module_source) {
            Ok(module) => {
                let mut temp = interpreter.fork();
                match temp.evaluate_module(&module) {
                    Ok(()) => {
                        if let Some(value) = temp.get_global(TEMP_BINDING) {
                            if let Some(stream) = value.as_stream() {
                                print_stream_preview(&stream, PREVIEW_TICKS);
                            } else {
                                println!("result is not a stream");
                            }
                        } else {
                            println!("no result produced");
                        }
                    }
                    Err(diags) => {
                        print_diagnostics(&diags, &module_source);
                    }
                }
            }
            Err(diags) => {
                print_diagnostics(&diags, &module_source);
            }
        }
    }
}

fn resolve_stream(interpreter: &Interpreter, name: &str) -> Option<Stream> {
    interpreter.get_sink(name).or_else(|| {
        interpreter
            .get_global(name)
            .and_then(|value| value.as_stream())
    })
}

fn print_stream_preview(stream: &Stream, ticks: usize) {
    if ticks == 0 {
        return;
    }
    for tick in 0..ticks {
        let value = stream.value_at(tick);
        println!("[{}] {}", tick, value.to_string_lossy());
    }
}

fn print_single_value(stream: &Stream, tick: usize) {
    let value = stream.value_at(tick);
    println!("[{}] {}", tick, value.to_string_lossy());
}

fn looks_like_definition(snippet: &str) -> bool {
    const KEYWORDS: [&str; 6] = ["value", "fn", "struct", "source", "sink", "let"];
    let trimmed = snippet.trim_start();
    KEYWORDS.iter().any(|keyword| trimmed.starts_with(keyword))
}

fn print_diagnostics(diags: &[Diagnostic], source: &str) {
    for diag in diags {
        println!("{}: {}", kind_label(diag.kind), diag.message);
        if let Some(span) = diag.span {
            if let Some((line, column, line_text)) = line_info(source, span) {
                println!(" --> line {}, column {}", line, column);
                println!("  {}", line_text);
                let pointer = " ".repeat(column.saturating_sub(1)) + "^";
                println!("  {}", pointer);
            }
        }
        for note in &diag.notes {
            println!(" note: {}", note);
        }
    }
}

fn kind_label(kind: DiagnosticKind) -> &'static str {
    match kind {
        DiagnosticKind::Lexer => "lexer error",
        DiagnosticKind::Parser => "parse error",
        DiagnosticKind::Type => "type error",
        DiagnosticKind::Eval => "eval error",
        DiagnosticKind::Io => "io error",
        DiagnosticKind::Internal => "internal error",
    }
}

fn line_info(source: &str, span: Span) -> Option<(usize, usize, String)> {
    let mut line = 1usize;
    let mut line_start = 0usize;

    for (idx, ch) in source.char_indices() {
        if idx >= span.start {
            break;
        }
        if ch == '\n' {
            line += 1;
            line_start = idx + ch.len_utf8();
        }
    }

    let line_text = source.lines().nth(line.saturating_sub(1))?.to_string();
    let column = source[line_start..span.start]
        .chars()
        .count()
        .saturating_add(1);

    Some((line, column, line_text))
}

fn derive_stream(interpreter: &Interpreter, expr: &str) -> Result<Stream, Vec<Diagnostic>> {
    let module_source = format!("let {} = ({});", TEMP_BINDING, expr);
    let module = parse_module(&module_source)?;
    let mut temp = interpreter.fork();
    temp.evaluate_module(&module)?;
    let value = temp.get_global(TEMP_BINDING).ok_or_else(|| {
        vec![Diagnostic::new(
            DiagnosticKind::Eval,
            "expression produced no value",
        )]
    })?;
    value.as_stream().ok_or_else(|| {
        vec![Diagnostic::new(
            DiagnosticKind::Eval,
            format!("expression '{}' did not evaluate to a stream", expr),
        )]
    })
}

fn print_help() {
    println!("Commands:");
    println!("  :help               Show this help message");
    println!("  :first <name>       Show stream value at tick 0");
    println!("  :at <name> <tick>   Show stream value at a specific tick");
    println!(
        "  :rest <name> [n]    Preview rest(<name>) for n (default {}) ticks",
        PREVIEW_TICKS
    );
    println!(
        "  :show <name> [n]    Preview up to n (default {}) ticks of a stream",
        PREVIEW_TICKS
    );
    println!("  :quit               Exit the REPL");
    println!();
    println!("Enter Vivid declarations (value, fn, struct, source, sink, let) or expressions.");
    println!("Finish input with an empty line to evaluate it.");
}
