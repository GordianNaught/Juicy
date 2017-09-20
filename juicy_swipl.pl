%% This file is used to compile Juicy using SWI-Prolog.
:- use_module(juicy,[start/1]).

% This is the entry point for the compiled compiler.
main(Arguments) :-
  start(Arguments).
