:- module(juicy_inference, [infer/6,infer_program/2]).

:- use_module(juicy_intrinsics, [intrinsic/3]).
:- use_module(utils).
:- use_module(juicy_global).

:- dynamic(toCompile/1).
:- dynamic(inferred/1).
:- dynamic(signature/3).
:- dynamic(signature_definition/2).

signature(apply,[func([ArgTypes],ReturnType),ArgTypes],ReturnType).

arguments_context(Args,ArgContext) :-
  arguments_context(Args,[],ArgContext).

arguments_context([],SoFar,SoFar) :- !.
arguments_context([arg(Type,Arg)|ArgR],SoFar,ArgContext) :-
  !,
  arguments_context(ArgR,[type(Arg,Type)|SoFar],ArgContext).
  
findSignature(Name,ArgTypes,ReturnType) :-
  signature(Name,ArgTypes,ReturnType),
  !.
  
findSignature(Name,ArgTypes,ReturnType) :-
  signature_definition(signature(Name,ArgTypes,ReturnType),Definition),
  !,
  infer(Definition),
  signature(Name,ArgTypes,ReturnType).

findSignature(Name,ArgTypes,ReturnType) :-
  intrinsic(Name,ArgTypes,ReturnType),
  !.

find(_,[]) :- !, fail.
find(Y,[Y|_]) :- !.
find(Y,[_|Rest]) :- !, find(Y,Rest).
  

%definition{name:Name,arguments:Arguments,returnType:ReturnType,body:Body}
infer_each([],[],[],Context,Context,_ReturnType) :- !.
infer_each([Element|Rest],
           [Inferred|InferredRest],
           [Type | RestTypes],
           Context,
           FinalContext,
           ReturnType) :-
  !,
  infer(Element, Inferred, Type, Context, NextContext,ReturnType),
  !,
  infer_each(Rest,InferredRest,RestTypes,NextContext,FinalContext,ReturnType).
           
%infer(Definition,_,_,_,_,_) :-
  %write(Definition), nl, fail.

% TODO
infer(Definition,
      definition(Name,Arguments,ReturnType,InferredBody),
      _Type,
      Context,
      _ContextAfter,
      ReturnType) :-
  Definition = definition(Name,Arguments,ReturnType,Body),
  ifVerbose((write(infer=[Name,Arguments,ReturnType]), nl)),
  arguments_context(Arguments,Context),
  arguments_types(Arguments,ArgumentTypes),
  infer_each(Body,
             InferredBody,
             _Types,
             [current(Name,ArgumentTypes,ReturnType)|Context],
             _NewContext,
             ReturnType),
  
  arguments_types(Arguments,ArgTypes),
  assert(signature(Name,ArgTypes,ReturnType)),
  assert(inferred(definition(Name,Arguments,ReturnType,InferredBody))),
  ifVerbose((write(done(Name)), nl)).
                              

infer(num(int(Num)),
      num(int(Num)),
      int,
      Context,
      Context,
      _Return) :- !.

infer(if(Condition,Body),
      if(InferredCondition,InferredBody),
      void,
      Context,
      Context,
      ReturnType) :-
  !,
  infer(Condition,InferredCondition,ConditionType,Context,NewContext,ReturnType),
  !,
  (ConditionType = bool ->
    infer_each(Body,InferredBody,_BodyType,NewContext,_NewerContext,ReturnType)
    ;
    format("condition of if statement has type of ~w, bool expected\n", [ConditionType]),
    fail).

% TODO
%infer(return(apply(var(_X),_Args)),
  %_InferredCode,
  %_ReturnType,
  %_Context,
  %_ContextAfter,
  %_FunctionReturnType) :-
  %write("uninplemented\n"),fail.
  
      
infer(return(Expr),
      return(InferredExpr),
      FunctionReturnType,
      Context,
      Context,
      FunctionReturnType) :-
  !,
  infer(Expr,InferredExpr,ExprReturnType,Context,_ContextAfter,FunctionReturnType),
  !,
  (ExprReturnType = FunctionReturnType ->
     find(current(Name,ArgumentTypes,FunctionReturnType),Context),
     assert(signature(Name,ArgumentTypes,FunctionReturnType))
     ;
     format("the return type of `~w' did not match the expected return type of `~w'\n",
            [ExprReturnType,FunctionReturnType])).


infer(var(X),var(X),Type,Context,Context,_FunctionReturn) :-
  (find(type(X,Type),Context),
   !
   ;
   format("unable to find type of ~w\n",[X]), !, fail).

%apply local variable

%can't pass intrinsic without it thunking
%so no need to worry about intrinsic_call
%and tail call prevention
infer(apply(var(X),Args),
      InferredCode,
      Type,
      Context,
      ContextAfter,
      FunctionReturnType) :-
  !,
  infer_each(Args,InferredArgs,ArgTypes,Context,ContextAfter,FunctionReturnType),
  (
    find(type(X,XType),Context) ->
      (
        XType = func(ArgTypes,Type) ->
          InferredCode = apply(var(X),InferredArgs)
          ;
          findSignature(apply, [XType | ArgTypes], Type),
          InferredCode = apply(var(apply),[var(X)|InferredArgs], Type)
      )
    ;
    intrinsic(X,ArgTypes,Type) ->
      InferredCode = apply_intrinsic(var(X),ArgTypes,Type,InferredArgs), !
    ;
    %apply function 
    findall(s(N,A,R),signature(N,A,R),S),
    findSignature(X,ArgTypes,Type) ->
      InferredCode = apply(var(X), InferredArgs), !
    ;
    find(current(X,ArgTypes,CurrentReturnType),Context) ->
    (
      ground(CurrentReturnType) ->
        InferredCode = apply(var(X),InferredArgs)
        ;
        format("unable to infer return type of function for recursive call, try starting with base case\n"),
        fail
    ), !
    ;
    format(
      "unable to find signature for function `~w` with arguments of types ~w~n",
    [X,ArgTypes]),
    !,
    fail
    %;
    %findall(s(N,A,R),signature(N,A,R),S),
    %write(S),nl,
    %format("unable to resolve ~w for arguments of types ~w\n", [X, ArgTypes]),
    %fail
  ).

infer(definition(Name,Arguments,ReturnType,Body),
      InferredCode,
      Type,
      Context,
      ContextAfter,
      FunctionReturnType) :-
  !,
  format("Unable to infer definition of `~w'.\n",[Name]),
  !,
  fail.

infer(Ast,
      InferredCode,
      Type,
      Context,
      ContextAfter,
      FunctionReturnType) :-
  !,
  Ast =.. [StatementType | _],
  format("Unable to infer `~w' expression/statement.\n", [StatementType]),
  !,
  fail.

partition(Group, Element, Predicate, Compatible, InCompatible) :-
  copy_term((Element,Predicate),(SatisfactoryElement,SatisfactoryPredicate)),
  findall(SatisfactoryElement,(member(SatisfactoryElement,Group), SatisfactoryPredicate),Compatible),
  copy_term((Element,not(Predicate)),(NonSatisfactoryElement,NotSatisfactoryPredicate)),
  findall(NonSatisfactoryElement,(member(NonSatisfactoryElement,Group), NotSatisfactoryPredicate),InCompatible).

groundDefinition(definition(Name,ArgTypes,_ReturnType,_Body)) :-
  ground((Name,ArgTypes)).

get_where(Element,Condition,Source,Result) :-
  (Condition =.. [',' | Conditions],
   Predicate =.. [',', member(Element,Source) | Conditions]
   ;
   Predicate = (member(Element,Source),Condition)),
  !,
  findall(Element,(member(Element,Source),Predicate), Result).
  
perform_each(Things,Thing,Performance) :-
  do_each(Things,Thing,Performance,_Result,_Results).
  
catalog_nonground(NongroundDefinitions) :-
  perform_each(NongroundDefinitions,
               definition(Name,Arguments,ReturnType,Body),
               (
                 arguments_types(Arguments,ArgTypes),
                 assert(signature_definition(signature(Name,ArgTypes,ReturnType),definition(Name,Arguments,ReturnType,Body))))).

catalog_ground(GroundDefinitions) :-
  perform_each(GroundDefinitions,
               definition(Name,Arguments,ReturnType,_Body),
               (
                 arguments_types(Arguments,ArgTypes),
                 assert(signature(Name,ArgTypes,ReturnType)))).

infer(Definition) :-
  infer(Definition,_,_,_,_,_).

infer_with_each(Definitions) :-
  perform_each(Definitions, Definition, infer(Definition)).

get_inferred(Inferrences) :-
  findall(Inferred,inferred(Inferred),Inferrences).

infer_program(Definitions, Inferrences) :-
  retractall(signature(_,_,_)),
  retractall(call_definition(_,_)),
  retractall(signature_definition(_,_)),
  retractall(inferred(_)),
  partition(Definitions,Definition,groundDefinition(Definition), GroundDefinitions, NongroundDefinitions),
  catalog_ground(GroundDefinitions),
  catalog_nonground(NongroundDefinitions),
  infer_with_each(GroundDefinitions),
  get_inferred(Inferrences),
  perform_each(
    Inferrences,
    definition(Name,Args,R,_),
    ifVerbose(format("~w ~w~w\n",[R,Name,Args]))).
  
