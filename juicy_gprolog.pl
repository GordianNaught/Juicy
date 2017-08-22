% This file is used to compile Juicy with GNU Prolog.

%op(1050, xfy, ->).

atom_string(A,B) :-
  atom_codes(A,B).

string_codes(A,B).

:- include('utils.pl').
:- include('juicy_tokenize.pl').
:- include('juicy_parse.pl').
:- include('juicy_intrinsics.pl').
:- include('juicy_inference.pl').
:- include('juicy_compile.pl').
:- include('juicy_optimize.pl').
:- include('juicy_forth_to_x86.pl').
:- include('assembly_optimize.pl').
:- include('juicy.pl').
:- initialization(main/1).
