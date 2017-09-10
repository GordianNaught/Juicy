                                          /¯/
           ⍟           ⍟                 / /
         /¯/ /¯/ /¯/ /¯/ /¯¯¯¯/ /¯/ /¯/ / /
        / / / /_/ / / / / /¯¯¯ / /_/ / /_/
     __/ /  \__,_/ /_/  \ ¯¯/  \__, /  ⍟
    /___/ ≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡ /____/ ≡≡≡≡≡≡≡≡≡≡≡

#### Juicy is a low-level, compiled, purely functional, highly generic language with inferred types and tail recursion.
#### The juicy compiler is written in Prolog.
#### Juicy is still in development and at this stage not suitable for use.

## Language Design Ethos
- #### Anything that can be decided statically at compile time, will be.
- #### Garbage collection is too expensive.
- #### Loops are bad.
- #### Mutation is bad.
- #### Inferrence is good.
- #### Interfaces are implicit.

# Table of Contents
* [Current Status](#current-status)
* [Immediate Plans](#immediate-plans)
* [Example Program](#example-program)
* [Example Output](#example-output)
* [Installation](#installation)
* [Use Instructions](#use-instructions)
* [Compiler Design](#compiler-design)
  * [Parsing](#parsing)
  * [Inferrence](#inferrence)
  * [Compilation](#compilation)


For other information not in this README, such as how to contribute, please see the [Wiki](https://github.com/GordianNaught/Juicy/wiki).

## Current Status
- [x] parser
- [x] type inferrence
- [x] byte code compiler
- [x] byte code peephole optimizer
- [x] asm generation
- [x] asm peephole optimization
- [x] emit x64
- [x] higher order functions
- [x] Makefile
- [ ] GNU Prolog support
- [ ] remove gcc dependecy in favor of `as` and `ld`
- [x] fix calling convention of generated main function
- [ ] add cuts in all places where non-determinism not needed
- [x] make verbose output optional
- [ ] add verbosity levels?
- [x] make timing of compilation optional
- [x] test cases
- [x] run tests when Makefile is ran
- [x] make internal names of functions carry signature information
- [ ] classes
- [ ] methods
- [ ] anonymous functions (lambdas)
- [ ] multiple return values
- [ ] inline functions
- [ ] many base types
- [ ] vector intrinsics
- [ ] many other intrinsics
- [ ] intrinsics as arguments to higher order functions
- [ ] implicit tagged unions
- [ ] pooling memory allocator
- [ ] constant folding
- [ ] macros
- [ ] parallelism support
- [ ] debug compilation
- [ ] debug annotations
- [ ] flag to remove tail recursion for debug compile traces
- [ ] make `man` page
- [ ] make installation move executable to bin

## Immediate Plans
- add more built in types and intrinsics
- remove superfluous inference from the compilation code
  - inference was separated from compilation as generics began to be implemented
- GNU Prolog support

## Example Program
```C
// accumulator is assumed to start
// at 1
factorialTail(x, acc) {
  if x == 0 {
    return acc;
  }
  // tail recursive call
  return factorialTail(x-1,x*acc);
}

// wrapper on tail recursive factorial
factorial(x) = factorialTail(x,1);

inc(x) = 1+x;

int main() = factorial(3);
```
## Example Output
```Unix Assembly
.section .data
ep_init:
block_buffers:
.section .bss
.section .text
.globl main

factorialTail_Nint_intP:
  movq   8(%rsp), %r8
  movq   $0, %r9
  xorq   %rax, %rax
  cmpq   %r9, %r8
  sete   %al
  negq   %rax
  movq   %rax, %r8
  test   %rax, %rax
  je   if1
  popq   %r8
  addq   $8, %rsp
  movq   %r8, %rax
  ret
  if1:
  movq   8(%rsp), %r8
  movq   $1, %r9
  subq   %r9, %r8
  movq   8(%rsp), %r9
  movq   0(%rsp), %r10
  imulq   %r10, %r9
  movq   %r8, 8(%rsp)
  movq   %r9, 0(%rsp)
  jmp   factorialTail_Nint_intP

factorial_NintP:
  movq   0(%rsp), %r8
  movq   $1, %r9
  movq   %r8, 0(%rsp)
  movq   %r9, %r8
  pushq   %r8
  jmp   factorialTail_Nint_intP

main_NP:
  movq   $3, %r8
  pushq   %r8
  jmp   factorial_NintP

main:
  pushq $returnLoc
  jmp main_NP
returnLoc:
  movl %eax, %edi
  call exit
```

## Installation
The juicy compiler is written in Prolog and intended to be compiled.
A Makefile is provided.
Compilation of the juicy compiler can currently be accomplished using SWI-Prolog.
GNU Prolog support is under development.
You will need SWI-Prolog and gcc installed to install juicy.

### Getting Prolog
#### Debian-based GNU/Linux)
##### SWI-Prolog
    sudo apt-get install swipl
##### GNU Prolog (not yet supported)
    sudo apt-get install gprolog
#### RPM-based GNU/Linux)
##### SWI-Prolog
    sudo yum install swipl
##### GNU Prolog (not yet supported)
    sudo yum install gprolog
    
### Building Juicy
#### Building with SWI-Prolog
    git clone THISREPO
    cd REPOFOLDER
    make swipl
#### Building with GProlog (not yet supported)
    git clone THISREPO
    cd REPOFOLDER
    make gprolog

## Use Instructions
    juicy -i INPUTFILE.juicy -o OUTPUTEXECUTABLE
    
## Compiler Design
### parsing
Parsing is accomplished using DCGs (Definite Clause Grammars).
### Inference
Type inference is accomplished by walking the ast using unification.

Type inference is not bi-directional. This is an explicit choice for efficiency reasons. Doing a tail call as the first return from a function causes difficulty in establishing the return type of the function. This could be worked around, but instead it is suggested that you put base cases before recursion, as this is good style regardless.

### Compilation

#### bytecode
After the inference pass, bytecode is generated. The bytecode is stack-based and remniscent of FORTH (the compiler first targetted FORTH). This leads to a callee cleans calling convention.

#### bytecode peephole optimizations
The bytecode is fed to a peephole optimizer.

#### asm
After the bytecode has been generated, pseudo-assembly is generated.
#### asm peephole optimizations
The pseudo assembly is fed to a peephole optimizer.
#### x64 asm
The pseudo assembly is then translated to the end result, x64 assembly.
