:- module(juicy_intrinsics, [intrinsic/3,intrinsic_instructions/4,intrinsic_instructions/3]).


intrinsic(+,[int,int],int).
intrinsic(-,[int,int],int).
intrinsic(*,[int,int],int).
intrinsic('/',[int,int],int).
intrinsic('%',[int,int],int).
intrinsic('<<',[int,int],int).
intrinsic('>>',[int,int],int).
intrinsic('<',[int,int],bool).
intrinsic('<=',[int,int],bool).
intrinsic('>',[int,int],bool).
intrinsic('>=',[int,int],bool).
intrinsic('==',[int,int],bool).
intrinsic('!=',[int,int],bool).

%define_intrinsic(chr,[int],
%intrinsic_compilAe(intrinsic(signature(

compSetInstruction('>=',setge).
compSetInstruction('>',setgt).
compSetInstruction('<=',setle).
compSetInstruction('<',setlt).
compSetInstruction('==',sete).
compSetInstruction('!=',setne).

intrinsic_instructions(intrinsic(Name,[ArgumentType]),_Target,_Source) :-
  format("unable to find instructions for intrinsic ~w on argument of type ~w~n",[Name,ArgumentType]),
  !,
  fail.
intrinsic_instructions(intrinsic(+,[int,int]),Source,Destination,[add(Source,Destination)]) :- !.

intrinsic_instructions(
  intrinsic(Comp,[int,int]),
  Source,
  Destination,
  [xorl(reg(eax),reg(eax)),
   cmp(Source,Destination),
   ConstructedPart,
   neg(reg(rax)),
   mov(reg(rax),Destination)]) :-

   compSetInstruction(Comp,CompInstruction),
   !,
   ConstructedPart =.. [CompInstruction,reg(al)].
intrinsic_instructions(
  intrinsic(+,[float,float]),
  Source,
  Destination,
  [fld(Source),fld(Destination),fadd(reg(st1),reg(st0)),fst(Destination)]) :- !.
intrinsic_instructions(intrinsic(-,[int,int]),Source,Destination,[sub(Source,Destination)]) :- !.
intrinsic_instructions(intrinsic(*,[int,int]),Source,Destination,[imul(Source,Destination)]) :- !.
intrinsic_instructions(
  intrinsic(/,[int,int]),
  Source,
  Destination,
  [xorr(reg(rdx),reg(rdx)),mov(Destination,reg(rax)),idiv(Source),mov(reg(rax),Destination)]) :- !.
intrinsic_instructions(
  intrinsic('%',[int,int]),
  Source,
  Destination,
  [xorr(reg(rdx),reg(rdx)),mov(Destination,reg(rax)),idiv(Source),mov(reg(rdx),Destination)]) :- !.
intrinsic_instructions(intrinsic('<<',[int,int]),Source,Destination,[sarl(Source,Destination)]) :- !.
intrinsic_instructions(intrinsic('>>',[int,int]),Source,Destination,[sarr(Source,Destination)]) :- !.
intrinsic_instructions(intrinsic(Name,ArgumentTypes),_Source,_Destination,_Code) :-
  format(
    "unable to find instructions for intrinsic ~w on arguments of types ~w~n",
    [Name,ArgumentTypes]),
  !,
  fail.
