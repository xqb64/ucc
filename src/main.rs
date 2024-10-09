use std::collections::{BTreeMap, BTreeSet};
use std::{collections::VecDeque, fs::File, path::PathBuf};

use anyhow::{bail, Result};
use structopt::StructOpt;

use ucc::ir::{IRInstruction, IRProgram, IRValue, Optimization};
use ucc::{
    codegen::gen::{build_asm_symbol_table, AsmType, Codegen},
    codegen::replace_pseudo::ReplacePseudo,
    codegen::fixup::Fixup,
    codegen::regalloc::RegAlloc,
    emitter::Emit,
    ir::{convert_symbols_to_tacky, IRNode, Irfy, Optimize},
    lexer::{Lexer, Token},
    loop_label::LoopLabel,
    parser::Parser,
    resolver::Resolve,
    typechecker::Typecheck,
};

fn main() {
    let opts = Opt::from_args();
    if let Err(e) = run(&opts) {
        eprintln!("ucc: {}", e);
        std::process::exit(1);
    }
}

fn run(opts: &Opt) -> Result<()> {
    let preprocessed = preprocess(&opts.path)?;
    let src = std::fs::read_to_string(preprocessed)?;

    let Some(tokens) = Lexer::new(src)
        .map(|token| {
            if token != Token::Error {
                Some(token)
            } else {
                None
            }
        })
        .collect::<Option<VecDeque<_>>>()
    else {
        bail!("failed to tokenize");
    };

    if opts.lex {
        println!("{:?}", tokens);
        std::process::exit(0);
    }

    let mut parser = Parser::new(tokens);
    let mut raw_ast = parser.parse()?;

    if opts.parse {
        println!("{:#?}", raw_ast);
        std::process::exit(0);
    }

    let mut variable_map = BTreeMap::new();
    let mut struct_map = BTreeMap::new();

    let cooked_ast = raw_ast
        .resolve(&mut variable_map, &mut struct_map)?
        .loop_label("")?
        .typecheck()?;

    if opts.validate {
        println!("{:#?}", cooked_ast);
        std::process::exit(0);
    }

    let mut tac = cooked_ast.irfy().unwrap();
    let tacky_defs = convert_symbols_to_tacky();

    let ir_prog = if let IRNode::Program(prog) = &mut tac {
        prog.static_vars = tacky_defs;

        prog
    } else {
        unreachable!()
    };

    let mut optimizations = vec![];

    if opts.fold_constants {
        optimizations.push(Optimization::ConstantFolding);
    }

    if opts.eliminate_unreachable_code {
        optimizations.push(Optimization::UnreachableCodeElimination);
    }

    if opts.propagate_copies {
        optimizations.push(Optimization::CopyPropagation);
    }

    if opts.eliminate_dead_stores {
        optimizations.push(Optimization::DeadStoreElimination);
    }

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
            .filter_map(|tl| match tl {
                IRNode::Function(f) => {
                    let vars = analyze(&f.body);
                    if !vars.is_empty() {
                        Some(vars)
                    } else {
                        None
                    }
                }

                _ => None,
            })
            .fold(BTreeSet::new(), |mut acc, hs| {
                acc.extend(hs);
                acc
            })
    }

    let aliased_pseudos = analyze_program(&optimized_prog);

    let mut asm_prog = optimized_prog
        .codegen()
        .reg_alloc(&aliased_pseudos)
        .replace_pseudo()
        .fixup(&BTreeSet::new());

    if opts.codegen {
        println!("{:#?}", asm_prog);
        std::process::exit(0);
    }

    let mut f = File::create(opts.path.with_extension("s"))?;
    asm_prog.emit(&mut f, &mut AsmType::Longword)?;

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
