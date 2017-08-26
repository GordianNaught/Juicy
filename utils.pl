:- module(
  utils,
  [
    appendAll/2,
    nCopies/3,
    do_each/5,
    append_strings_delimited/3,
    allSame/1,
    arguments_types/2,
    cleanLabel/2
  ]).
:- meta_predicate(do_each(?,?,0,?,?)).
:- use_module(library(dcg/basics)).

arguments_types([],[]) :- !.
arguments_types([arg(Type,_Arg)|Rest],[Type|RestTypes]) :-
  !,
  arguments_types(Rest,RestTypes).
  
allSame([]) :- !.
allSame([F|R]) :- allSame(F,R).
allSame(_,[]) :- !.
allSame(F,[F|R]) :- allSame(F,R).

appendAll(Lists,Answer) :- appendAll(Lists,[],AnswerR), reverse(AnswerR,Answer).

appendAll([],Sofar,Sofar) :- !.
appendAll([[]|R],SoFar,Answer) :-
  !,
  appendAll(R,SoFar,Answer).
appendAll([[A|Ar]|R],Sofar,Answer) :-
  !,
  appendAll([Ar|R],[A|Sofar],Answer).

append_strings_delimited([],"",_Delimiter) :- !.
append_strings_delimited([String],String,_Delimiter) :- !.
append_strings_delimited([String|OtherStrings],Result,Delimiter) :-
  !,
  append_strings_delimited(OtherStrings,Rest,Delimiter),
  format(string(Result),"~w~w~w",[String,Delimiter,Rest]).

nCopies(N,Thing,Answer) :-
  nCopies(N,Thing,[],Answer).

nCopies(0,_,CopiesSoFar,CopiesSoFar) :- !.
nCopies(N,Thing,CopiesSoFar,Answer) :-
  K is N - 1,
  nCopies(K,Thing,[Thing|CopiesSoFar],Answer).

do_each([],
        _SourceElement,
        _Template,
        _ResultElement,
        []) :- !.
do_each([SourceElement1|SourceRest],
        SourceElement,
        Template,
        ResultElement,
        [ResultElement1|ResultRest]) :-
  copy_term((SourceElement,Template,ResultElement),
            (SourceElement1,Template1,ResultElement1)),
  !,
  call(Template1),
  !,
  do_each(SourceRest,SourceElement,Template,ResultElement,ResultRest).
cleanLabel([Letter|Rest]) -->
  [Letter],
  {char_type(Letter,alpha), !},
  cleanLabel(Rest).
cleanLabel(['_'|Rest]) -->
  [','],
  !,
  cleanLabel(Rest).
cleanLabel(['_','_'|Rest]) -->
  ['_'],
  !,
  cleanLabel(Rest).
cleanLabel([Encoded|Rest]) -->
  [Letter], 
  {
    char_code(Letter,Code),
    !,
    EncodedCode is 65+(Code mod 26),
    char_code(Encoded, EncodedCode)
  },
  cleanLabel(Rest).
cleanLabel([],[],[]).
cleanLabel(Given,CleanLabel) :-
  format(string(String),"~w",Given),
  string_chars(String,Chars),
  cleanLabel(CleanChars,Chars,[]),
  string_chars(CleanLabel,CleanChars).
