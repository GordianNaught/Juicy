:- module(juicy_intrinsics, 
          [
            intrinsic/4,
            intrinsic_instructions/4,
            intrinsic_instructions/3
          ]).

% intrinsic/3 is used to tell the inference engine
% the signature of an existing intrinsic function.

% intrinsic(FUNCTION_NAME, ARGUMENT_TYPES, RETURN_TYPE).
intrinsic(float,[char],float,1).
intrinsic(float,[int],float,1).
intrinsic(float,[float],float,1).
intrinsic(float,[float],float,1).


intrinsic(int,[char],int,1).
intrinsic(int,[float],int,1).
intrinsic(int,[int],int,1).

intrinsic(char,[int],char,1).
intrinsic(char,[float],char,1).
intrinsic(char,[char],char,1).

intrinsic(byte,[int],byte,1).
intrinsic(byte,[float],byte,1).
intrinsic(byte,[char],byte,1).

intrinsic(byte,[ascii],byte,1).

intrinsic(ascii,[byte],ascii,1).

intrinsic(emit,[ascii],ascii,1).

intrinsic(+,[byte,byte],byte,1).
intrinsic(-,[byte,byte],byte,1).
intrinsic(*,[byte,byte],byte,1).
intrinsic('|',[byte,byte],byte,1).
intrinsic('/',[byte,byte],byte,1).
intrinsic('%',[byte,byte],byte,1).
intrinsic('<<',[byte,byte],byte,1).
intrinsic('>>',[byte,byte],byte,1).

intrinsic('==',[byte,byte],byte,1).
intrinsic('!=',[byte,byte],byte,1).
intrinsic('>',[byte,byte],byte,1).
intrinsic('<',[byte,byte],byte,1).
intrinsic('<=',[byte,byte],byte,1).
intrinsic('>=',[byte,byte],byte,1).

intrinsic('index',[generic(vector,[T]),int],T,1).
intrinsic('length',[generic(vector,[T])],T,1).

intrinsic(*,[byte,byte],byte,1).

intrinsic(+,[int,int],int,1).
intrinsic(-,[int,int],int,1).
intrinsic(*,[int,int],int,1).
intrinsic('/',[int,int],int,1).
intrinsic('%',[int,int],int,1).
intrinsic('<<',[int,int],int,1).
intrinsic('>>',[int,int],int,1).
intrinsic('<',[int,int],bool,1).
intrinsic('<=',[int,int],bool,1).
intrinsic('>',[int,int],bool,1).
intrinsic('>=',[int,int],bool,1).
intrinsic('==',[int,int],bool,1).
intrinsic('!=',[int,int],bool,1).

sizeof(bit,1).
sizeof(byte,8).
sizeof(ascii,8).
sizeof(char,32).
sizeof(int,64).
sizeof(float,64).

compSetInstruction('>=',setge).
compSetInstruction('>',setgt).
compSetInstruction('<=',setle).
compSetInstruction('<',setlt).
compSetInstruction('==',sete).
compSetInstruction('!=',setne).

% intrinsic_instructions/3 is used to implement an
% intrinsic function that takes 1 arguments.

% The result is output into the same register that
% is input as the argument (TARGET_REGISTER).

% intrinsic_instructions(intrinsic(FUNCTION_NAME,
%                                  ARGUMENT_TYPES,
%                                  RETURN_COUNT),
%                        TARGET_REGISTER,
%                        CODE_TO_EMIT)
intrinsic_instructions(
  intrinsic(emit,[ascii],1),
  Target,
  [
    mov(Target,reg(rax)),
    xor(reg(rdi),reg(rdi)),
    movsbl(reg(al),reg(edi)),
    call(putchar)
    %movsbq(reg(al),Target)
  ]).

intrinsic_instructions(
  intrinsic(ascii,[byte],1),
  _Target,
  []).
  
intrinsic_instructions(
  intrinsic(byte,[ascii],1),
  _Target,
  []).

intrinsic_instructions(
  intrinsic(byte,[int],1),
  Target,
  [
    mov(Target,reg(rax)),
    xor(Target,Target),
    movsbq(reg(al),Target)
  ]).

intrinsic_instructions(
  intrinsic(Name,[ArgumentType],_ReturnCount),
  _Target,
  _Code) :-
  format("unable to find instructions for intrinsic ~w on argument of type ~w~n",[Name,ArgumentType]),
  !,
  fail.
  
% intrinsic_instructions/4 is used to implement an
% intrinsic function that takes 2 arguments.

% The two arguments are suppled on the SOURCE_REGISTER
% and DESTINATION_REGISTER, but the result overwrites
% the value in the DESTINATION_REGISTER.

% intrinsic_instructions(intrinsic(FUNCTION_NAME,
%                                  ARGUMENT_TYPES,
%                                  RETURN_COUNT),
%                        SOURCE_REGISTER,
%                        DESTINATION_REGISTER,
%                        CODE_TO_EMIT).
intrinsic_instructions(intrinsic(+,[int,int],1),
                       Source,
                       Destination,
                       [add(Source,Destination)]) :- !.

intrinsic_instructions(
  intrinsic(Comp,[int,int],1),
  Source,
  Destination,
  [xor(reg(rax),reg(rax)),
   cmp(Source,Destination),
   ConstructedPart,
   neg(reg(rax)),
   mov(reg(rax),Destination)]) :-

   compSetInstruction(Comp,CompInstruction),
   !,
   ConstructedPart =.. [CompInstruction,reg(al)].
   
intrinsic_instructions(
  intrinsic(+,[float,float],1),
  Source,
  Destination,
  [fld(Source),fld(Destination),fadd(reg(st1),reg(st0)),fst(Destination)]) :- !.

intrinsic_instructions(
  intrinsic(-,[int,int],1),
  Source,
  Destination,
  [sub(Source,Destination)]) :- !.

intrinsic_instructions(
  intrinsic(*,[int,int],1),
  Source,
  Destination,
  [
    mov(Destination,reg(rax)),
    imul(Source),
    mov(reg(rax),Destination)
  ]) :- !.
  
intrinsic_instructions(
  intrinsic(/,[int,int],1),
  Source,
  Destination,
  [xor(reg(rdx),reg(rdx)),mov(Destination,reg(rax)),idiv(Source),mov(reg(rax),Destination)]) :- !.
  
intrinsic_instructions(
  intrinsic('%',[int,int],1),
  Source,
  Destination,
  [xorr(reg(rdx),reg(rdx)),mov(Destination,reg(rax)),idiv(Source),mov(reg(rdx),Destination)]) :- !.
intrinsic_instructions(intrinsic('<<',[int,int],1),Source,Destination,[sarl(Source,Destination)]) :- !.
intrinsic_instructions(intrinsic('>>',[int,int],1),Source,Destination,[sarr(Source,Destination)]) :- !.
intrinsic_instructions(intrinsic(Name,ArgumentTypes,_ReturnCount),_Source,_Destination,_Code) :-
  format(
    "unable to find instructions for intrinsic ~w on arguments of types ~w~n",
    [Name,ArgumentTypes]),
  !,
  fail.
