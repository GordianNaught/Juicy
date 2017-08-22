:- module(utils, [appendAll/2,nCopies/3,do_each/5,append_strings_delimited/3,allSame/1]).
:- meta_predicate(do_each(?,?,0,?,?)).

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
