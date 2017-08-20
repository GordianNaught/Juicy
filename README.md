JUICY
=====

#### Juicy is a low-level, compiled, purely functional, highly generic language with inferred types and tail recursion.
#### The juicy compiler is written in SWI-Prolog.
#### Juicy is still in development and at this stage not suitable for use.

## Language Design Ethos
- #### Anything that can be decided statically at compile time, will be.
- #### Garbage collection is too expensive.
- #### Loops are bad.
- #### Mutation is bad.
- #### Inferrence is good.
- #### Interfaces are implicit.

## Current Status
- [x] parser
- [x] type inferrence
- [x] byte code compiler
- [x] byte code peephole optimizer
- [x] asm generation
- [x] asm peephole optimization
- [x] emit x64
- [x] higher order functions
- [ ] fix calling convention of generated main function
- [ ] test cases written
- [ ] make internal names of functions carry signature information
- [ ] classes
- [ ] methods
- [ ] anonymous functions (lambdas)
- [ ] many base types
- [ ] vector intrinsics
- [ ] many intrinsics
- [ ] intrinsics as arguments to higher order functions
- [ ] implicit tagged unions
- [ ] pooling memory allocator
- [ ] constant folding
- [ ] macros

## Immediate plans
- fix calling convention of generated main function
  - without this the generated assembly cannot be ran without modification
- make internal names of functions carry signature information
  - without this a function cannot be used generically as two different signatures

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
factorialTail:
  movq   8(%rsp), %r8
  movq   $0, %r9
  xorl   %eax, %eax
  cmpq   %r9, %r8
  sete   %al
  negq   %rax
  movq   %rax, %r8
  test   %rax, %rax
  je   if2
  popq   %r8
  addq   $8, %rsp
  movq   %r8, %rax
  ret
  if2:
  movq   8(%rsp), %r8
  movq   $1, %r9
  subq   %r9, %r8
  movq   8(%rsp), %r9
  movq   0(%rsp), %r10
  imulq   %r10, %r9
  movq   %r8, 8(%rsp)
  movq   %r9, 0(%rsp)
  jmp   factorialTail

factorial:
  movq   0(%rsp), %r8
  movq   $1, %r9
  movq   %r8, 0(%rsp)
  movq   %r9, %r8
  pushq   %r8
  jmp   factorialTail

main:
  movq   $3, %r8
  pushq   %r8
  jmp   factorial
```

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
