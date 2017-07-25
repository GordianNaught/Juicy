:- module(juicy_forth_to_string, [forth_to_string/2]).

:- use_module(utils).

forth_term_to_text(Term,Chars) :-
  term_string(Term,S),
  string_codes(S,Chars).

forth_to_text([],[],[]) :- !.
forth_to_text([Chars],[Element],[]) :- forth_term_to_text(Element,Chars), !.
forth_to_text([Chars,` `|Rest]) -->
  [Element],
  {forth_term_to_text(Element,Chars), !},
  forth_to_text(Rest).

forth_to_string(Tokens, String) :-
  forth_to_text(TextParts, Tokens, []),
  appendAll(TextParts, Text),
  string_codes(String, Text).
