pub mod cfg;
pub mod codegen;
pub mod emitter;
pub mod ir;
pub mod lexer;
pub mod loop_label;
pub mod parser;
pub mod resolver;
pub mod typechecker;
pub mod optimizer {
    pub mod copy_propagation;
    pub mod unreachable_code_elimination;
    pub mod dead_store_elimination;
    pub mod constant_folding;
}
