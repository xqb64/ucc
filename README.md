# ucc

This is a Rust implementation of the C compiler from Nora Sandler's book.

## How it works

At the forefront is the compiler driver, which orchestrates the compilation process---from lexing and parsing, through several stages of semantic analysis, such as variable resolution, loop labeling, and type checking. After generating an intermediate representation (IR), the compiler passes it through an iterative optimization pipeline. This pipeline leverages forward and backward data-flow analysis, liveness analysis, and address-taken analysis to implement optimizations such as constant folding, unreachable code elimination, copy propagation, and dead store elimination. Finally, it generates and optimizes the x86_64 assembly code using a graph coloring-based register allocator with conservative coalescing, before emitting the optimized code.

## Status

- [x] char (in both unsigned and signed variants)
- [ ] short
- [x] int (in both unsigned and signed variants)
- [x] long (in both unsigned and signed variants)
- [ ] float
- [x] double
- [x] void
- [x] if/else
- [ ] switch
- [x] while
- [x] do while
- [x] for
- [x] break
- [x] continue
- [ ] ConsideredHarmful
- [x] static
- [x] extern
- [x] arrays
- [x] structs
- [x] sizeof
- [ ] enums
- [ ] unions
- [ ] typedef

## License

Licensed under the MIT license.