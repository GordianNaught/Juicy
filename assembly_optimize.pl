%% This module is a peephole optimizer for the pseudo-assembly
%% intermediate representation.
:- module(assembly_optimize, [assembly_optimize/3]).
:- use_module(juicy_global).

optimize_step([mov(A,A)|Rest],Rest).
optimize_step([mov(A,B),mov(B,A)|Rest],[mov(A,B)|Rest]) :-
  A \= stack_pointer,
  B \= stack_pointer.
optimize_step([push(reg(X)),pop(reg(X))|Rest],Rest).

optimize_step([push(reg(A)),pop(reg(B))|Rest],
              [mov(reg(A),reg(B)) | Rest]).

optimize_step([mov(reg(X),stack(0)),pop(reg(X))|Rest],
              [add(1*cell_size,stack_pointer)|Rest]).

%if statement optimization
optimize_step([mov(B,A),test(A,'$'(0)),jng(Label)|Rest],
              [test(B,'$'(0)),jng(Label)|Rest]).

optimize_pass([],[]).
optimize_pass(Given,Optimized) :-
  optimize_step(Given,Improved),
  !,
  optimize_pass(Improved,Optimized).
optimize_pass([S|R],[S|R1]) :-
  optimize_pass(R,R1).

assembly_optimize(0,Given,Given) :-
  ifVerbose((write(optimization=Given), nl)),
  !.
assembly_optimize(Level,Given,Result) :-
  NewLevel is Level - 1,
  ifVerbose((write(optimization=Given), nl)),
  optimize_pass(Given,Intermediate),
  (Intermediate == Given ->
    Result = Intermediate
    ;
    assembly_optimize(NewLevel,Intermediate,Result)).
