use std::{
    collections::{BTreeMap, BTreeSet, HashSet, VecDeque},
    fs::File,
    path::PathBuf,
};

use structopt::StructOpt;
use unicode_width::UnicodeWidthStr;

use ucc::{
    codegen::{
        fixup::Fixup,
        gen::{build_asm_symbol_table, AsmType, Codegen},
        regalloc::RegAlloc,
        replace_pseudo::ReplacePseudo,
    },
    emitter::emit::Emit,
    ir::gen::{
        convert_symbols_to_tacky, IRInstruction, IRNode, IRProgram, IRValue, Irfy, Optimization,
        Optimize,
    },
    lexer::lex::{Lexer, Span, TokenKind},
    parser::{ast::Type, recursive_descent::Parser},
    semantics::{
        collecting_cases::SwitchCaseCollect,
        label_checker::LabelCheck,
        loop_label::{LabelContext, LabelKind, LoopLabel},
        resolver::Resolve,
        typechecker::Typecheck,
    },
    util::error::{ErrorKind, Result, UccError},
};

macro_rules! collect_enabled {
    ($opts:ident, $($field:ident => $variant:expr),+ $(,)?) => {
        vec![
            $($opts.$field.then_some($variant)),+
        ].into_iter().flatten().collect::<Vec<_>>()
    };
}

fn main() {
    let opts = Opt::from_args();
    if let Err(_) = run(&opts) {
        std::process::exit(1);
    }
}

fn run(opts: &Opt) -> Result<()> {
    let preprocessed = preprocess(&opts.path)?;
    let src = std::fs::read_to_string(preprocessed)?;

    let tokens: VecDeque<_> =
        Lexer::new(src.clone()).try_fold(VecDeque::new(), |mut acc, token| {
            if token.kind == TokenKind::Error {
                print_errctx(&src, &token.span);

                Err(UccError {
                    kind: ErrorKind::Lex,
                    msg: format!("Failed to tokenize"),
                    span: token.span,
                })
            } else {
                acc.push_back(token);
                Ok(acc)
            }
        })?;

    if opts.lex {
        println!("{:?}", tokens);
        std::process::exit(0);
    }

    let mut parser = Parser::new(tokens);
    let raw_ast = match parser.parse() {
        Ok(ast) => ast,
        Err(err) => {
            print_errctx(&src, &err.span);
            return Err(err);
        }
    };

    if opts.parse {
        println!("{:#?}", raw_ast);
        std::process::exit(0);
    }

    let mut variable_map = BTreeMap::new();
    let mut struct_map = BTreeMap::new();

    let cooked_ast = raw_ast
        .resolve(&mut variable_map, &mut struct_map)
        .map_err(|err| {
            print_errctx(&src, &err.span);
            err
        })?
        .loop_label(LabelContext {
            innermost: LabelKind::None,
            loop_label: "",
            switch_label: "",
        })
        .map_err(|err| {
            print_errctx(&src, &err.span);
            err
        })?
        .label_check(&mut HashSet::new(), "")
        .map_err(|err| {
            print_errctx(&src, &err.span);
            err
        })?
        .typecheck()
        .map_err(|err| {
            println!("{:?}: {}\n", err.kind, err.msg);
            print_errctx(&src, &err.span);
            err
        })?
        .collect_switch_cases(&mut vec![], &Type::Dummy)
        .map_err(|err| {
            print_errctx(&src, &err.span);
            err
        })?;

    if opts.validate {
        println!("{:#?}", cooked_ast);
        std::process::exit(0);
    }

    let mut tac = cooked_ast.irfy("").unwrap();
    let (static_variables, static_constants) = convert_symbols_to_tacky();

    let ir_prog = if let IRNode::Program(prog) = &mut tac {
        prog.static_vars = static_variables;
        prog.static_constants.extend(static_constants);

        prog
    } else {
        unreachable!()
    };

    let optimizations = collect_enabled!(
        opts,
        fold_constants => Optimization::ConstantFolding,
        eliminate_unreachable_code => Optimization::UnreachableCodeElimination,
        propagate_copies => Optimization::CopyPropagation,
        eliminate_dead_stores => Optimization::DeadStoreElimination,
    );

    let optimized_prog = ir_prog.optimize(optimizations);

    if opts.tacky {
        println!("tac: {:#?}", optimized_prog);
        std::process::exit(0);
    }
    build_asm_symbol_table();

    fn analyze_program(program: &IRProgram) -> BTreeSet<String> {
        fn analyze(instrs: &[IRInstruction]) -> BTreeSet<String> {
            instrs
                .iter()
                .filter_map(|instr| match instr {
                    IRInstruction::GetAddress {
                        src: IRValue::Var(v),
                        dst: _,
                    } => Some(v.clone()),
                    _ => None,
                })
                .collect()
        }

        program
            .functions
            .iter()
            .filter_map(|f| {
                let vars = analyze(&f.body);
                if !vars.is_empty() {
                    Some(vars)
                } else {
                    None
                }
            })
            .fold(BTreeSet::new(), |mut acc, hs| {
                acc.extend(hs);
                acc
            })
    }

    let aliased_pseudos = analyze_program(&optimized_prog);

    let asm_prog = optimized_prog
        .codegen()
        .reg_alloc(&aliased_pseudos)
        .replace_pseudo()
        .fixup(&BTreeSet::new());

    if opts.codegen {
        println!("{:#?}", asm_prog);
        std::process::exit(0);
    }

    let mut f = File::create(opts.path.with_extension("s"))?;
    asm_prog.emit(&mut f, AsmType::Longword)?;

    if opts.s {
        std::process::exit(0);
    }

    if opts.c {
        std::process::Command::new("gcc")
            .arg("-c")
            .arg(opts.path.with_extension("s"))
            .arg("-o")
            .arg(opts.path.with_extension("o"))
            .status()?;
        std::process::exit(0);
    }

    let mut final_executable_cmd = std::process::Command::new("gcc");

    final_executable_cmd
        .arg("-o")
        .arg(opts.path.with_extension(""))
        .arg(opts.path.with_extension("s"));

    if let Some(ref lib) = opts.l {
        final_executable_cmd.arg("-l").arg(lib);
    }

    final_executable_cmd.status()?;

    Ok(())
}

fn preprocess(path: &PathBuf) -> Result<PathBuf> {
    let new_path = path.with_extension("i");

    std::process::Command::new("gcc")
        .arg("-E")
        .arg("-P")
        .arg(path)
        .arg("-o")
        .arg(new_path.clone())
        .status()?;

    Ok(new_path)
}

fn print_errctx(source: &str, span: &Span) {
    let start_byte = nth_char_byte_offset(source, span.start);
    let end_byte = nth_char_byte_offset(source, span.end);

    let mut line_start_bytes = vec![0];
    for (i, ch) in source.char_indices() {
        if ch == '\n' {
            line_start_bytes.push(i + 1);
        }
    }
    line_start_bytes.push(source.len());

    let offending_line_idx = match line_start_bytes.binary_search(&start_byte) {
        Ok(i) => i,
        Err(i) => i - 1,
    };

    let start_line = offending_line_idx.saturating_sub(3);
    let end_line = (offending_line_idx + 3).min(line_start_bytes.len() - 2);

    for i in start_line..=end_line {
        let line_start = line_start_bytes[i];
        let mut line_end = line_start_bytes[i + 1];
        if line_end > 0 && source.as_bytes()[line_end - 1] == b'\n' {
            line_end -= 1;
        }
        if line_end < line_start {
            line_end = line_start; // ensure empty slice, not panic
        }
        let line = &source[line_start..line_end];
        let line_num = i + 1;

        println!("{:>4} | {}", line_num, line);

        if i == offending_line_idx {
            let prefix = &source[line_start..start_byte];
            let highlight = &source[start_byte..end_byte];

            let prefix_width = UnicodeWidthStr::width(prefix);
            let highlight_width = UnicodeWidthStr::width(highlight).max(1);

            println!(
                "     | {}{}",
                " ".repeat(prefix_width),
                "^".repeat(highlight_width)
            );
        }
    }
}

fn nth_char_byte_offset(s: &str, n: usize) -> usize {
    s.char_indices().nth(n).map(|(i, _)| i).unwrap_or(s.len())
}

#[derive(Debug, StructOpt)]
struct Opt {
    path: PathBuf,

    #[structopt(name = "lex", long)]
    lex: bool,

    #[structopt(name = "parse", long)]
    parse: bool,

    #[structopt(name = "validate", long)]
    validate: bool,

    #[structopt(name = "tacky", long)]
    tacky: bool,

    #[structopt(name = "codegen", long)]
    codegen: bool,

    #[structopt(name = "c", short)]
    c: bool,

    #[structopt(name = "l", short)]
    l: Option<String>,

    #[structopt(name = "s", short)]
    s: bool,

    #[structopt(name = "fold-constants", long)]
    fold_constants: bool,

    #[structopt(name = "eliminate-unreachable-code", long)]
    eliminate_unreachable_code: bool,

    #[structopt(name = "propagate-copies", long)]
    propagate_copies: bool,

    #[structopt(name = "eliminate-dead-stores", long)]
    eliminate_dead_stores: bool,
}
