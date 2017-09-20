:- module(
  juicy_global,
  [
    assert_verbose/0,
    verbose/0,
    ifVerbose/1
  ]).

:- dynamic(verbose).
:- meta_predicate(ifVerbose(?)).

assert_verbose() :- assert(verbose).
ifVerbose(Code) :- verbose, !, Code.
ifVerbose(_Code).
