:- module(juicy_parse, [parse/2]).

:- use_module(library(dcg/basics)).
:- use_module(juicy_global).
:- use_module(utils).

plural_greedy(Thing,[T|Ts]) --> {E=..[Thing,T]}, E,  plural_greedy_more(Thing,Ts).
plural_greedy_more(Thing,[T|Ts]) --> {E=..[Thing,T]}, E, plural_greedy_more(Thing,Ts).
plural_greedy_more(_,[]) --> [].

program(Definitions) --> plural_greedy(definition,Definitions).
arguments(Arguments) --> ['('], arguments_list(Arguments), [')'].
arguments([]) --> ['('], [')'].
arguments_list([A|R]) --> argument(A), plural_greedy_more(delimited_argument,R).
delimited_argument(A) --> [','], argument(A).
variable(var(Name)) --> [var(Name)].
argument(arg(T,A)) --> type(T), [var(A)].
argument(arg(_Unbound,A)) --> [var(A)].
type(generic(T,Args)) --> [var(T)], ['<'],delimited_types(Args),['>'].
type(T) --> [var(T)].
delimited_types([F|Rest]) --> type(F), plural_greedy_more(delimited_type,Rest).
delimited_type(T) --> [','], type(T).
signature(Name,Arguments,T) -->
  type(T), ([var(Name)];math_op(Name)), arguments(Arguments);
  ([var(Name)];math_op(Name)), arguments(Arguments).
definition(definition(Name,Arguments,T,[return(Expr)])) -->
  signature(Name,Arguments,T),
  ['='], expr(Expr), [';'].
definition(definition(Name,Arguments,T,Code)) -->
  signature(Name,Arguments,T),
  %{write(definition(definition(Name,Arguments,[First|Rest])))},
  block(Code).
  %{write([First|Rest])}.
statement_semi(Expr) --> [';'], statement(Expr).
assignment(assign(var(Dest), Source)) --> [var(Dest)], ['='], expr(Source).
math_op(Op) --> [Op],
  {member(Op,['+','-','/','*','%','<<','>>','|','&','&&',
              '||','==','!=','>=','>','<','<='])}.
%statement(Expr) --> expr(Expr).
statement(for(Start,Finish,LoopVariable,[Statement])) -->
  ['for'], variable(LoopVariable), ['from'], expr(Start),
  ['to'], expr(Finish), ['do'], statement(Statement).
statement(for(Start,Finish,LoopVariable,Code)) -->
  ['for'], variable(LoopVariable), ['from'], expr(Start),
  ['to'], expr(Finish), block(Code).
statement(while(Expr,Statement)) -->
  ['while'], expr(Expr), ['do'], statement(Statement).
statement(while(Expr,Code)) -->
  ['while'], expr(Expr), ['do'], block(Code).
statement(if(Condition,[TrueCode])) -->
  ['if'], expr(Condition), ['then'], expr(TrueCode), [';'].
statement(if(Condition,TrueCode)) -->
  ['if'], expr(Condition), block(TrueCode).
statement(if(Condition,TrueCode,FalseCode)) -->
  ['if'], expr(Condition),
  block(TrueCode),
  ['else'],
  block(FalseCode).
  
block_part(Statement) --> statement(Statement).
block_part(Expr) --> expr(Expr), [';'].

block([]) --> ['{'], ['}'].
block(Parts) -->
  ['{'], 
    plural_greedy_more(block_part, Parts),
    %{write([First|Rest])},
  ['}'].
delimited_statement(Expr) --> [';'], statement(Expr).
delimited_statements(Exprs) --> plural_greedy_more(statement, Exprs).
%statement(nop) --> [";"].
%expr(E) --> comparison(E).
%expr(ternary(Condition,TrueExpr,FalseExpr)) -->
  %expr(Condition), ['?'], expr(TrueExpr), [':'], expr(FalseCode).
expr(return(Expr)) --> ['return'], expr(Expr).
expr(return) --> ['return'].
expr(Assignment) --> assignment(Assignment).
expr(gen(Start,Finish,Index,Solution)) -->
  ['get'], expr(Solution),
  ['for'], variable(Index),
  ['from'], expr(Start),
  ['to'], expr(Finish).
expr(apply(var(Op),[A,B])) --> value(A), math_op(Op), expr(B).
expr(post_apply(F,Args)) --> ['<'], variable(F), ['>'], arguments(Args).
expr(apply(F,[])) --> variable(F), ['('], [')'].
expr(apply(V,[])) --> value(V), ['('], [')'].
expr(apply(F,[Arg|Rest])) -->
  variable(F),
   ['('], expr(Arg), delimited_array_elements(Rest), [')'].
expr(apply(F,[Arg|Rest])) -->
  value(F),
   ['('], expr(Arg), delimited_array_elements(Rest), [')'].
expr(freeze(F)) --> ['freeze'], ['('], variable(F), [')'].
expr(E) --> value(E).
%expr(array(Elements)) -->
value(num(N)) --> [num(N)].
value(V) --> variable(V).
value(Expr) --> ['('], expr(Expr), [')'].
value(apply(var(index),[Var,I])) -->
  variable(Var), ['['], expr(I), [']'].
value(apply(var(index),[Expr,I])) -->
  ['('], expr(Expr), [')'], ['['], expr(I), [']'].
value(array([])) --> ['['], [']'].
value(array([Element|Rest])) -->
  ['['], expr(Element), delimited_array_elements(Rest), [']'].
delimited_array_elements(Elements) -->
  plural_greedy_more(delimited_array_element, Elements).
delimited_array_element(Element) -->
  [','], expr(Element).
%value(
%expr(execute(F,Args)) --> 

parse(Tokens,Ast) :-
  program(Ast,Tokens,[]), !,
  ifVerbose((write('AST'=Ast),nl)).
