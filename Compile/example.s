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

