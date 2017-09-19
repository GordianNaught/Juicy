:- module(juicy_tokenize, [tokenize/2]).

:- use_module(library(dcg/basics)).
:- use_module(utils).

plural_greedy(Thing,[T|Ts]) -->
  {E=..[Thing,T]}, E,  plural_greedy_more(Thing,Ts).
  
plural_greedy_more(Thing,[T|Ts]) -->
  {E=..[Thing,T]}, E, !, plural_greedy_more(Thing,Ts).
  
plural_greedy_more(_,[]) --> [].

b --> blanks.
alpha(A) --> [A], {char_type(A,alpha)}.
word(W) --> plural_greedy(alpha,Codes), {atom_codes(W,Codes)}.
num(num(float(N))) -->
  `.`, digits(Decimals), !,
  {
    append(`0.`,Decimals,Codes),
    number_codes(Codes,N)
  }.
num(num(float(N))) -->
  digits(Ds), `.`, digits(Decimals), !,
  {
    appendAll([Ds,`.`,Decimals],Codes),
    number_codes(N,Codes)
  }.
num(num(int(N))) --> digits(Ds), {Ds\=[],number_codes(N,Ds)}.
variable(var(X)) --> word(X).%, {not(reserved(X))}.

tokenize(String,Tokens) :-
  string_codes(String,Codes),
  tokenize(Tokens,Codes,[]),
  !.
token(def,"def").
token(for,"for").
token(if,"if").
token(then,"then").
token('?',"?").
token(':',":").
token('}',"}").
token('{',"{").
token(';',";").
token(']',"]").
token('[',"[").
token(from,"from").
token(to,"to").
token(return,"return").
token(get,"get").
token(do,"do").
token(while,"while").
token('(',"(").
token(')',")").
token(',',",").
token('<<',"<<").
token('>>',">>").
token('|',"|").
token('&',"&").
token('&&',"&&").
token('||',"||").
token('==',"==").
token('!=',"!=").
token('>=',">=").
token('>',">").
token('<',"<").
token('<=',"<=").
token('=',"=").
token('+',"+").
token('-',"-").
token('/',"/").
token('*',"*").
token('%',"%").

tokenize_comment(Tokens) -->
  string(_Commented), "*/", !, b, tokenize(Tokens).
  
tokenize([],[],_).
tokenize(Tokens) -->
  b, "/*", tokenize_comment(Tokens).
tokenize(Tokens) -->
  b, "//", string_without("\n",_RestOfLine), b, tokenize(Tokens).
tokenize(Tokens) -->
  b, "#!", string_without("\n",_RestOfLine), b, tokenize(Tokens).
tokenize([N|Rest]) --> b, num(N), b, tokenize(Rest).
tokenize([var(Name)|Rest]) -->
  b, variable(var(Name)), b, {not(token(Name,_))}, tokenize(Rest).
tokenize([Token|Rest]) --> {token(Token,String)}, b, String, b, tokenize(Rest).
