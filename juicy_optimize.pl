%% This module is a peephole optimizer for Juicy bytecode.
:- module(juicy_optimize, [optimize/3]).
:- use_module(juicy_global).

% These make the optimizer aware of some commutative
% operations so that it can sometimes reorder instructions.
commutative(intrinsic(OpName,[_Type1,_Type2],1)) :-
  !,
  commutative(OpName).
commutative(+).
commutative('*').
commutative('|').
commutative(&).
commutative(and).
commutative(or).

% This makes the optimizer aware of what a function taking only
% one argument and returning only one value looks like.
monadic(func(_Name,_,1,1,_ReturnLabel)).

% This makes the optimizer aware of what functions taking two
% arguments and returning only one value look like.
binary(func(_Name,_,2,1,_ReturnLabel)).
binary(intrinsic(OpName,[_Type1,_Type2],1)) :- !, binary(OpName).
binary('<<').
binary(+).
binary(-).
binary(*).
binary('|').
binary(&).
binary('&&').
binary('||').
binary('%').
binary('/').

% These optimization steps dictate optimizing transformations
% the peephole optimizer is able to make on the bytecode.

% optimize_step(Given, Optimized).
optimize_step(
  [dup,tailcall(Name,Types,1)|Rest],
  [tailcall(Name,Types,1)|Rest]
).
optimize_step(
  [over,over,tailcall(Name,Types,2)|Rest],
  [tailcall(Name,Types,2)|Rest]
).
optimize_step(
  ['2dup',tailcall(Name,Types,2)|Rest],
  [tailcall(Name,Types,2)|Rest]
).
optimize_step(
  ['2dup',swap,tailcall(Name,Types,2)|Rest],
  [swap,tailcall(Name,Types,2)|Rest]
).
optimize_step(
  [dup,tailcall(Name,Types,1)|Rest],
  [tailcall(Name,Types,1)|Rest]
).
optimize_step([dup,func(Name,Types,1,RetLabel),nip|Rest],
              [func(Name,Types,1,RetLabel)|Rest]).
optimize_step(['2dup',Binary,nip,nip|Rest],[Binary|Rest]) :-
  binary(Binary).
optimize_step([dup,num(Num1),num(Num2),Op1,Op2,nip|Rest],
              [num(Num1),num(Num2),Op1,Op2|Rest]):-
  binary(Op1),
  binary(Op2).
optimize_step([dup,over,Binary,nip,exit|Rest],[swap,Binary,exit|Rest]) :-
  binary(Binary).
optimize_step([dup,over,Binary,nip],[dup,Binary]) :-
  binary(Binary).
optimize_step([swap,Op|Rest],[Op|Rest]) :-
  commutative(Op).
optimize_step([dup,num(Num),Binary,nip,exit|Rest],
              [num(Num),Binary,exit|Rest]) :-
  binary(Binary).
optimize_step([dup,2,pick|Rest],['2dup',swap|Rest]).
optimize_step([1,pick,1,pick|Rest],['2dup'|Rest]).
%optimize_step([X,pick,0,pick,Op|Rest],[dup,Xinc,pick,Op|Rest]) :-
  %commutative(Op),
  %Xinc is X+1.
optimize_step([dup,num(Num),Op,nip|Rest],[num(Num),Op|Rest]) :-
  binary(Op).
optimize_step([num(Num),1,pick,Op|Rest],[dup,num(Num),Op|Rest]) :-
  commutative(Op).
optimize_step([1,pick,nip,nip|Rest],[drop|Rest]).
optimize_step([over,nip|Rest],[dup|Rest]).
optimize_step(['I',drop|Rest],Rest).
optimize_step([num(_Num),drop|Rest],Rest).
optimize_step([over,num(Num),Op,nip|Rest],[drop,dup,num(Num),Op|Rest]):-
  binary(Op).
  
optimize_step([num(int(0)),intrinsic(+,[int,int])|Rest],Rest).
optimize_step([num(int(0)),intrinsic(-,[int,int])|Rest],Rest).
optimize_step([num(int(1)),intrinsic(*,[int,int])|Rest],Rest).
optimize_step([num(int(1)),intrinsic('/',[int,int])|Rest],Rest).

optimize_step([num(float(0)),intrinsic(+,[float,float])|Rest],Rest).
optimize_step([num(float(0)),intrinsic(-,[float,float])|Rest],Rest).
optimize_step([num(float(1)),intrinsic(*,[float,float])|Rest],Rest).
optimize_step([num(float(1)),intrinsic('/',[float,float])|Rest],Rest).

optimize_step([do,loop|Rest],[drop,drop|Rest]).
optimize_step([dup,drop|Rest],Rest).
%optimize_step([dup,over,Op|Rest],Rest).
optimize_step([0,pick,nip|Rest],Rest).
optimize_step([0,pick|Rest],[dup|Rest]).
optimize_step([1,pick|Rest],[over|Rest]).
optimize_step([0,pick|Rest],[dup|Rest]).
optimize_step([0,pick|Rest],[dup|Rest]).
optimize_step([exit],[]).
optimize_pass([],[]).
optimize_pass(Given,Optimized) :-
  optimize_step(Given,Improved),
  !,
  optimize_pass(Improved,Optimized).
optimize_pass([S|R],[S|R1]) :-
  optimize_pass(R,R1).

optimize(0,Given,Given) :-
  ifVerbose((write(optimization=Given), nl)),
  !.
optimize(Level,Given,Result) :-
  NewLevel is Level - 1,
  ifVerbose((write(optimization=Given), nl)),
  optimize_pass(Given,Intermediate),
  (Intermediate == Given ->
    Result = Intermediate
    ;
    optimize(NewLevel,Intermediate,Result)).


