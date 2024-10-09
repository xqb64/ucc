pub mod cfg;
pub mod emitter;
pub mod ir;
pub mod lexer;
pub mod loop_label;
pub mod parser {
    pub mod ast;
    pub mod recursive_descent;
}
pub mod resolver;
pub mod typechecker;
pub mod optimizer {
    pub mod constant_folding;
    pub mod copy_propagation;
    pub mod dead_store_elimination;
    pub mod unreachable_code_elimination;
}
pub mod codegen {
    pub mod fixup;
    pub mod gen;
    pub mod regalloc;
    pub mod replace_pseudo;
}
