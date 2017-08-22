% this file is used to compile Juicy using SWI-Prolog
:- use_module(juicy,[start/1]).

main(Arguments) :-
  write(Arguments), nl,
  time(start(Arguments)).
