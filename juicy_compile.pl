:- module(juicy_compile, [compile_program/2]).

:- use_module(juicy_optimize, [optimize/3]).
:- use_module(juicy_forth_to_x86, [forth_to_x86/4]).
:- use_module(library(gensym), [gensym/2]).
:- use_module(juicy_intrinsics, [intrinsic/4]).
:- use_module(utils).
:- use_module(juicy_global).

arguments_context(Args,ArgContext) :-
  arguments_context(Args,[],ArgContext,1).

arguments_context([],SoFar,SoFar,_) :- !.
arguments_context([arg(Type,Arg)|ArgR],SoFar,ArgContext,N) :-
  I is 0 - N,
  K is N + 1,
  arguments_context(ArgR,
                    [type(Arg,Type),assign(Arg,I)|SoFar],
                    ArgContext,
                    K).

findLocation(X,[assign(X,L)|_],L) :- !.
findLocation(X,[_|R],L) :-
  findLocation(X,R,L).

getType(var(_N),[],_Type) :- !, fail.
getType(var(N),[type(N,Type)|_],Type) :- !.
getType(var(N),[_|Rest],Type) :- getType(var(N),Rest,Type).

getCurrentSignature(_Signature,[]) :- !, fail.
getCurrentSignature(Signature,[currentSignature(Signature)|_]) :- !.
getCurrentSignature(Signature,[_|Rest]) :- !, getCurrentSignature(Signature,Rest).

getFuncType(F,Context,func(ArgTypes,ReturnType)) :-
  getType(F,Context,Type),
  Type =.. [generic,func,ArgsAndReturn],
  append(ArgTypes,[ReturnType],ArgsAndReturn).

compile_each([],[],Context,Context,Offset,Offset,_,[]) :- !.
compile_each([return(X)|_],[Code],Context,Context,Offset,0,N,[T]) :-
  !,
  compile(return(X),Code,Context,_,Offset,0,N,T).

compile_each([Expr|Exprs],[Code|Codes],Context,NewContext,Offset,NewOffset,N,[T|RT]) :-
  compile(Expr,Code,Context,Context2,Offset,Offset2,N,T), !,
  compile_each(Exprs,Codes,Context2,NewContext,Offset2,NewOffset,N,RT).

assign_each([],[],[]).
assign_each([Expr|Exprs],[assign(Variable,Expr)|Assignments],[Variable|Variables]) :-
  genvar(Variable),
  assign_each(Exprs,Assignments,Variables).

% may delete these later
containsAssignment(assign(_,_)) :- !.
containsAssignment([]) :- fail.
containsAssignment([F|R]) :-
  containsAssignment(F);
  containsAssignment(R).
containsAssignment(Code) :-
  functor(Code,_,Args),
  containsAssignment(Args).

max(A,B,A) :- A>=B, !.
max(_,B,B).
% signature(Name,ArgTypes,ReturnType)

findFunction(
  signature(Name,ArgTypes,ReturnType,ReturnCount),
  [signature(Name,ArgTypes,ReturnType,ReturnCount)|_]) :- !.
findFunction(signature(Name,ArgTypes,_,_),[]) :-
  !,
  format("unable to find function `~w' with argument types ~w~n",[Name,ArgTypes]),
  fail.
findFunction(Signature,[_|Rest]):-
  findFunction(Signature,Rest).
  
start_context(
  [
    type('I',int),
    signature(length,[generic(vector,[_])],int)
  ]).

genvar(var(gen(VarName))) :- gensym(var,VarName), !.
genLabel(Name,label(Label)) :- gensym(Name,Label), !.

compile(A,_,_,_,_,_,_,_) :-
  ifVerbose((write(compile(A)), nl)),
  fail.

%fix
compile(gen(Start,Finish,var(Index),Solution),
        Code,
        Context,
        Context,
        Offset,
        NewOffset,
        N,
        generic(vector,[T])) :-
  compile(Finish,CFinish,Context,Context1,Offset,Offset1,N,int),
  compile(Start,CStart,Context1,Context2,Offset1,Offset2,N,int),
  Offset3 is Offset2 - 2,
  K is N+1,
  compile(
    Solution,
    CSolution,
    [assign(Index, index)|Context2],
    _,
    Offset3,
    NewOffset,
    K,
    T),
  appendAll([CFinish,CStart,[do],CSolution,[loop]],Code).

%no assignment in declaration
compile(for(Start,Finish,var(Index),Statements),
        Code,
        Context,
        Context,
        Offset,
        Offset,
        N,
        void) :-
  compile(Finish,CFinish,Context,Context1,Offset,Offset1,N,int),
  compile(Start,CStart,Context1,Context2,Offset1,_,N,int),
  K is N+1,
  compile_each(
    [assign(var(Index),'I')| Statements],
    Codes,
    Context2,
    _,
    Offset3,
    NewOffset,
    K,
    _),
  StackChange is NewOffset - Offset3,
  % allowing for return statement
  % code cause negative stack change
  max(StackChange,0,StackMovement),
  nCopies(StackMovement,drop,CleanBody),
  appendAll(Codes,LoopBodyCode),
  appendAll([CFinish,CStart,[do],LoopBodyCode,CleanBody,[loop]],Code).

%no assignment in declaration
compile(if(Condition,Body),Code,Context,Context,Offset,Offset,N,void) :-
  compile(Condition,ConditionCode,Context,Context,Offset,Offset1,N,bool),
  % this ensures that no assignments happened
  Offset1 is Offset+1,
  !,
  compile_each(Body,BodyPartsCompiled,Context,_,Offset,OffsetAfterBody,N,_),
  StackChange is OffsetAfterBody-Offset,
  % allowing for return statement
  % code cause negative stack change
  max(StackChange,0,StackMovement),
  nCopies(StackMovement,drop,CleanBody),
  appendAll(BodyPartsCompiled,BodyCompiled),
  genLabel(if,Label),
  appendAll([ConditionCode,[if(Label,StateHolder)],BodyCompiled,CleanBody,[then(Label,StateHolder)]],Code).

%no assignment
%compile(ternary(Condition,TrueExpr,FalseExpr),Code,Context,Context,Offset,Offset1,N) :-
  %compile(Condition,ConditionCode,Context,Context,Offset,Offset1,N),
  %Offset1 is Offset+1,
  %!,
  %compile(TrueExpr,TrueCode,Context,Context,Offset,Offset1,N),
  %compile(FalseExpr,FalseCode,Context,Context,Offset,Offset1,N),
  %appendAll([ConditionCode,[if],TrueCode,[else],FalseCode,[then]],Code).

compile('I',['I'],C,C,O,NO,_,int) :- NO is O + 1.

compile(definition(Name,Arguments,ReturnType,ReturnCount,Body),
        [function(Name,ArgumentTypes,Asm)],
        StartContext,
        StartContext,
        Offset,
        _,
        N,
        void) :-
  arguments_types(Arguments,ArgumentTypes),
  arguments_context(Arguments,ArgContext),
  get_signatures([definition(Name,Arguments,ReturnType,ReturnCount,Body)],
                 [Signature]),
  appendAll([ArgContext,[currentSignature(Signature)],StartContext],FullContext),
  length(Arguments,ArgCount),
  OffsetWithArgs is Offset + ArgCount,
  compile_each(Body,Results,FullContext,_,OffsetWithArgs,_EndOffset,N,_),
  appendAll(Results,ResultsAppended),
  ifVerbose(format("~n    Bytecode optimizations:~n~n")),
  optimize(20,ResultsAppended,OptimizedCode),
  forth_to_x86(ArgCount,OptimizedCode,Asm,ReturnCount),
  !.
        
% higher order funcall
% no assignment in args
% checked by not allowing extra stack shifting
compile(apply(var(F),Args),Compiled,Context,Context,Offset,NewOffset,N,ReturnType) :-
  findLocation(F,Context,_),
  append(Args,[var(F)],Parts),
  compile_each(Parts,CompiledParts,Context,Context,Offset,OffsetBeforeApplication,N,ArgTypes),
  !,
  getFuncType(F,Context,func(ArgTypes,ReturnType)),
  appendAll(CompiledParts,CompiledWithoutExec),
  length(Args,ArgCount),
  genLabel(F,Label),
  appendAll([[num(int(Label))],CompiledWithoutExec,[execute(ArgCount,Label)]],Compiled),
  NewOffset is (ReturnCount-1)+(OffsetBeforeApplication-ArgCount).

% higher order funcall
compile(apply(var(F),Args,ReturnCount),
        Compiled,
        Context,
        NewContext,
        Offset,
        NewOffset,
        N,
        ReturnType) :-
  findLocation(F,Context,_),
  !,
  % fix
  assign_each(Args,Assignments,Variables),
  appendAll([Assignments,Variables,[var(F)]],ToCompile),
  compile_each(ToCompile,CompiledBeforeFuncCallParts,Context,NewContext,Offset,OffsetBeforeApplication,N,ReturnTypes),
  append(_,ArgTypes,ReturnTypes),
  length(Args,ArgCount),
  length(ArgTypes,ArgCount),
  !,
  getFuncType(F,Context,func(ArgTypes,ReturnType,ReturnCount)),
  appendAll(CompiledBeforeFuncCallParts,CompiledBeforeFuncCall),
  append(CompiledBeforeFuncCall,[execute],Compiled),
  NewOffset is (ReturnCount-1)+(OffsetBeforeApplication-ArgCount).

% regular funcall
% no assignment in args
% checked by not allowing extra stack shifting
compile(apply(var(F),Args,ReturnCount),
        Compiled,
        Context,
        Context,
        Offset,
        NewOffset,
        N,
        ReturnType) :-
  genLabel(F,LabelName),
  compile_each([push(LabelName)|Args],
                CompiledArgs,
                Context,
                Context,
                Offset,
                OffsetBeforeApplication,
                N,
                [_Label|ArgTypes]),
  !,
  findFunction(signature(F,ArgTypes,ReturnType,ReturnCount),Context),
  appendAll(CompiledArgs,CompiledWithoutFuncall),
  length(Args,ArgCount),
  appendAll(
    [
      CompiledWithoutFuncall,
      [func(F,ArgTypes,ArgCount,ReturnCount,LabelName)]
    ],
    Compiled),
  NewOffset is OffsetBeforeApplication-(ArgCount-(ReturnCount-1)).

% intrinsic funcall
% no assignment in args
% checked by not allowing extra stack space
compile(apply_intrinsic(var(F),ArgTypes,ReturnType,ReturnCount,Args),
        Compiled,
        Context,
        Context,
        Offset,
        NewOffset,
        N,
        ReturnType) :-
  compile_each(Args,
               CompiledArgs,
               Context,
               Context,
               Offset,
               OffsetBeforeApplication,
               N,
               ArgTypes),
  !,
  appendAll(CompiledArgs,CompiledArgsAppended),
  append(CompiledArgsAppended,[intrinsic(F,ArgTypes,ReturnCount)],Compiled),
  length(Args,ArgCount),
  %write(NewOffset is OffsetBeforeApplication - (ArgCount-1)), nl,
  NewOffset is OffsetBeforeApplication - (ArgCount-ReturnCount).
  
% wrapper used to throw off compiler pattern matching
compile(identity(X),Code,Context,NewContext,Offset,NewOffset,N,ReturnType) :-
  compile((X),Code,Context,NewContext,Offset,NewOffset,N,ReturnType).

% regular tail funcall
% no assignment in args
% checked by not allowing extra stack shifting
tail(apply(var(F),Args,ReturnCount),
     Compiled,
     Context,
     Context,
     Offset,
     NewOffset,
     N,
     ReturnType) :-

  compile_each(
    Args,
    CompiledArgs,
    Context,
    Context,
    Offset,
    OffsetBeforeApplication,
    N,
    ArgTypes),
  !,
  findFunction(signature(F,ArgTypes,ReturnType,ReturnCount),Context),
  appendAll(CompiledArgs,CompiledWithoutFuncall),
  length(Args,ArgCount),
  appendAll(
    [
      CompiledWithoutFuncall,
      [tailcall(F,ArgTypes,ArgCount,ReturnCount)]
    ],
    Compiled),
  NewOffset is OffsetBeforeApplication-(ArgCount-(ReturnCount-1)).
  
% regular funcall
compile(apply(var(F),Args,ReturnCount),
        Compiled,
        Context,
        NewContext,
        Offset,
        NewOffset,
        N,
        ReturnType) :-
  assign_each(Args,Assignments,Variables),
  appendAll([Assignments,Variables],ToCompile),
  compile_each(ToCompile,CompiledBeforeFuncCallParts,Context,NewContext,Offset,OffsetBeforeApplication,N,ReturnTypes),
  append(_Extra,ArgTypes,ReturnTypes),
  length(Args,ArgCount),
  length(ArgTypes,ArgCount),
  !,
  findFunction(signature(F,ArgTypes,ReturnType),Context),
  appendAll(CompiledBeforeFuncCallParts,CompiledBeforeFuncCall),
  append(CompiledBeforeFuncCall,[func(F,ArgTypes,ArgCount,ReturnCount)],Compiled),
  NewOffset is OffsetBeforeApplication-(ArgCount-ReturnCount).

%compile(apply(F,_Args),_Compiled,_Context,_NewContext,_Offset,_NewOffset,_N,_Type) :-
  %!,
  %format("unable to find function `F'~n with proper argument types",[F]),
  %fail.
%compile(apply(F,Args),Compiled,Context,NewContext,Offset,NewOffset) :-
  
compile(var(X),Compiled,Context,Context,Offset,NewOffset,_,Type) :-
  findLocation(X, Context, LocationBeforeOffset),
  !,
  getType(var(X),Context,Type),
  (LocationBeforeOffset == index ->
     Compiled=['I']
     ;
     Location is LocationBeforeOffset + Offset,
     Compiled = [Location, pick])
  ,
  NewOffset is Offset + 1.

% get execution pointer of declared function
compile(var(X),
        ['[\']',function(X,ArgTypes,ReturnType)],
        Context,
        Context,
        Offset,
        NewOffset,
        _,
        func(ArgTypes,ReturnType)) :-
  findFunction(signature(X,ArgTypes,ReturnType),Context),
  !,
  NewOffset is Offset + 1.
  
compile(var(VariableName),_,Context,_,_,_,_,_) :-
  getCurrentSignature(signature(CurrentFunction,_,_,_),Context),
  format("Unable to find variable `~w' in function `~w'~n",[VariableName,CurrentFunction]),
  fail.

%fix compile each to handle assignments inside
compile(array(Elements),Compiled,Context,Context2,Offset,NewOffset,N,generic(vector,[Type])) :-
  length(Elements,ElementCount),
  compile_each(Elements,MakeElements,Context,Context2,Offset,_,N,[Type|RestTypes]),
  allSame([Type|RestTypes]),
  appendAll(MakeElements,ElementCode),
  appendAll([ElementCode,[ElementCount,makeArray]],Compiled),
  NewOffset is Offset + 1.

% no assignment allowed?
compile(math(-,num(int(0)),Y),
        Code,
        Context,
        Context1,
        Offset,
        Offset1,
        N,
        int) :-
  compile(Y,CompiledY,Context,Context1,Offset,Offset1,N,int),
  !,
  append(CompiledY,[intrinsic(-,[int])],Code).

% no assignment allowed?
%compile(apply(Op,[X,Y]), Compiled, Context, Context2, Offset, NewOffset, N, ReturnType) :-
  %compile(X,CompiledX,Context,Context1,Offset,Offset1,N,TypeX),
  %compile(Y,CompiledY,Context1,Context2,Offset1,Offset2,N,TypeY),
  %NewOffset is Offset2 - 1,
  %OperationInstruction =  intrinsic(Op,[TypeX,TypeY]),
  %!,
  %(returnType(OperationInstruction,ReturnType) ->
    %appendAll([CompiledX,CompiledY,[OperationInstruction]],Compiled)
    %;
    %write("unable to find return type for: "),
    %write(OperationInstruction),
    %nl,
    %fail).

compile(return(apply(F,A,RC)),
        Compiled,
        Context,
        NewContext,
        Offset,
        0,
        LoopCount,
        Type) :-
  !,
  tail(apply(F,A,RC),Compiled,Context,NewContext,Offset,_NewOffset,LoopCount,Type),
  getCurrentSignature(signature(Name,_,NeededReturnType,CRC),Context),
  (NeededReturnType \= Type ->
     format("type `~w' does not match expected return type of `~w' for function `~w'~n",
            [Type,NeededReturnType,Name]),
     fail
     ;
     1=1
     ).

compile(return, Compiled, Context, Context, Offset, 0, LoopCount, Type) :-
  getCurrentSignature(signature(Name,_,NeededReturnType,_RC),Context),
  (NeededReturnType \= Type ->
     format("type `~w' does not match expected return type of `~w' for function `~w'~n",
            [Type,NeededReturnType,Name]),
     fail
     ;
     DropCount is Offset,
     nCopies(DropCount, drop, Drops),
     nCopies(LoopCount, unloop, Unloops),
     appendAll([Drops,Unloops,[exit]], Compiled)).

compile(return(Expr),
        Compiled,
        Context,
        NewContext,
        Offset,
        0,
        LoopCount,
        Type) :-
  compile(Expr, CompiledExpr, Context, NewContext, Offset, NewOffset, LoopCount, Type),
  getCurrentSignature(signature(Name,_,NeededReturnType,RC),Context),
  (NeededReturnType \= Type ->
     format("type `~w' does not match expected return type of `~w' for function `~w'~n",
            [Type,NeededReturnType,Name]),
     fail
     ;
     CopyCount is (NewOffset - 1) + (RC-1),
     nCopies(CopyCount, nip, Nips),
     nCopies(LoopCount, unloop, Unloops),
     appendAll([CompiledExpr,Nips,Unloops,[exit]], Compiled)).

% index no assignments
compile(index(VectorExpr,IndexExpr),Compiled,Context,Context,Offset,NewOffset,N,Type) :-
  compile(IndexExpr,CompiledIndexExpr,Context,Context,Offset,Offset1,N,int),
  % no assignment
  Offset1 is Offset + 1,
  compile(VectorExpr,CompiledVectorExpr,Context,Context,Offset1,Offset2,N,generic(vector,[Type])),
  % no assignment
  Offset2 is Offset1 + 1,
  appendAll([CompiledIndexExpr,CompiledVectorExpr,[getindex]],Compiled),
  NewOffset is Offset + 1.

compile(assign(var(Dest),Source),
        CompiledSource,
        Context,
        [type(Dest,Type),assign(Dest,VarIndex)|NewContext],
        Offset,
        OffsetAfterExpr,
        N,
        Type) :-
  compile(Source, CompiledSource, Context, NewContext, Offset, OffsetAfterExpr, N, Type),
  (Type \= void ->
    true
    ;
    format("unable to assign void type to variable `~w'\n",[Dest]),
    fail),
  VarIndex is 0-OffsetAfterExpr.

compile(push(label(Name)),[num(int(Name))],Context,Context,Offset,NewOffset,_,int) :-
  !,
  NewOffset is Offset + 1.
  
compile(num(int(Num)),[num(int(Num))],Context,Context,Offset,NewOffset,_,int) :-
  NewOffset is Offset + 1.
  
compile(num(float(Num)),[num(float(Num))],Context,Context,Offset,NewOffset,_,float) :-
  NewOffset is Offset + 1.
  
compile(char(Char),[Code],Context,Context,Offset,NewOffset,_,char) :-
  char_code(Char,Code),
  NewOffset is Offset + 1.

get_arg_types([],[]) :- !.
get_arg_types([arg(Type,_Name)|RestArgs],[Type|RestTypes]) :-
  get_arg_types(RestArgs,RestTypes).
  
get_signatures([],[]) :- !.
get_signatures([
  definition(Name,Args,ReturnType,ReturnCount,_Code)|Rest],
  [signature(Name,ArgTypes,ReturnType,ReturnCount)|RestSignatures]) :-

  get_arg_types(Args,ArgTypes),
  get_signatures(Rest,RestSignatures).

compile_part(Ast,Signatures,Code) :-
  compile(Ast,Code,Signatures,_,0,_,0,void).

compile_program(Definitions,Codes) :-
  get_signatures(Definitions,Signatures),
  do_each(Definitions,
          Definition,
          juicy_compile:compile_part(Definition,Signatures,Code),
          Code,
          Codes).

%compile(str(String),Compiled,Context,Context,Offset,NewOffset) :-
  %NewOffset is Offset + 1.














