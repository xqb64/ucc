use std::collections::HashMap;
use std::{collections::VecDeque, fs::File, path::PathBuf};

use anyhow::{bail, Result};
use structopt::StructOpt;

use ucc::codegen::{Codegen, Fixup, ReplacePseudo};
use ucc::emitter::Emit;
use ucc::ir::Irfy;
use ucc::lexer::{Lexer, Token};
use ucc::loop_label::Label;
use ucc::parser::Parser;
use ucc::resolver::Resolve;
use ucc::typechecker::typecheck_block;

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
    let mut ast = parser.parse()?;

    if opts.parse {
        println!("{:?}", ast);
        std::process::exit(0);
    }

    let mut variable_map = HashMap::new();
    let validated_ast = ast.resolve(&mut variable_map)?;

    let mut symbol_table = HashMap::new();

    let labeled_ast = validated_ast.label(String::new())?;
    typecheck_block(&labeled_ast, &mut symbol_table)?;


    if opts.validate {
        println!("{:?}", labeled_ast);
        std::process::exit(0);
    }

    let tac = labeled_ast.irfy().unwrap();

    if opts.tacky {
        println!("{:?}", tac);
        std::process::exit(0);
    }

    let mut asm_prog = tac.codegen().replace_pseudo().fixup();

    if opts.codegen {
        println!("{:?}", asm_prog);
        std::process::exit(0);
    }

    let mut f = File::create(opts.path.with_extension("s"))?;

    asm_prog.emit(&mut f)?;

    if opts.c {
        std::process::Command::new("gcc")
            .arg("-c")
            .arg(opts.path.with_extension("s"))
            .arg("-o")
            .arg(opts.path.with_extension("o"))
            .status()?;
        std::process::exit(0);
    }

    std::process::Command::new("gcc")
        .arg("-o")
        .arg(opts.path.with_extension(""))
        .arg(opts.path.with_extension("s"))
        .status()?;

    Ok(())
}

fn preprocess(path: &PathBuf) -> Result<PathBuf> {
    let new_path = path.clone().with_extension("i");

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
}
