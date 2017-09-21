%% This module is for translating Juicy bytecode into x64 assembly.
:- module(juicy_forth_to_x86, [forth_to_x86/4]).
:- use_module(utils).
:- use_module(library(clpfd)).
:- use_module(assembly_optimize, [assembly_optimize/3]).
:- use_module(juicy_intrinsics).
:- use_module(juicy_global).
:- use_module(translate_instruction).

size_of(int,8).
size_of(float,8).
size_of(byte,1).
size_of(bool,1).
size_of(char,4).

% Here I was thinking about operations that need to be supported.
% Many of these operations are FORTH inspired like the bytecode itself.

% nip rot swap dup unloop 2dup tuck over
% + - * / mod << >>
% ['] name
% execute
% makearray
% allocate
% length
% indexarray
% return
% tail call recognition
% do loop
% if else then

register_count(3).
register_name(N,reg(N)).

% This functor represents current state of the registers and stack.
% It should help encapsulate the available registers and such on
% a per architecture basis in the future when more architectures are
% supported.

% state(UtilizedRegisters,
%       RegisterCount,
%       RegisterShift,
%       StackOffset,
%       RegisterNames).

% This predicate shows the expected starting state upon entering a function.
start_state(
  ArgCount,
  state(0,8,0,ArgCount,registers(r8,r9,r10,r11,r12,r13,r14,r15))).


push_all_registers([],
                   state(0,
                         RegisterCount,
                         RegisterShift,
                         StackOffset,
                         RegisterNames),
                   state(0,
                         RegisterCount,
                         RegisterShift,
                         StackOffset,
                         RegisterNames)) :-
  !.

push_all_registers([push(Register) | RestInstructions],
                   state(UtilizedRegisters,
                         RegisterCount,
                         RegisterShift,
                         StackOffset,
                         RegisterNames),
                   NewState) :-
  RegisterLocation is UtilizedRegisters - 1,
  pick(Register,
       RegisterLocation,
       state(UtilizedRegisters,
             RegisterCount,
             RegisterShift,
             StackOffset,
             RegisterNames)),
  RegistersLeft = RegisterLocation,
  NewOffset is StackOffset + 1,
  NewRegisterShift is RegisterShift + 1,
  !,
  push_all_registers(
    RestInstructions,
    state(RegistersLeft,
          RegisterCount,
          NewRegisterShift,
          NewOffset,
          RegisterNames),
    NewState).

force_to_register_and_get_first(
  Register,
  [pop(Register)],
  state(0,RegisterCount,RegisterShift,StackOffset,RegisterNames),
  state(1,RegisterCount,RegisterShift,NewStackOffset,RegisterNames)) :-

  !,
  get_first_register(Register,
                     state(0,
                           RegisterCount,
                           RegisterShift,
                           StackOffset,
                           RegisterNames)),
  NewStackOffset is StackOffset - 1.

force_to_register_and_get_first(Register,[],State,State) :-
  !, pick(Register,0,State).

get_register(reg(Name),
             N,
             state(UtilizedRegisters,
                   RegisterCount,
                   RegisterShift,
                   _,
                   RegisterNames)) :-
  N =< UtilizedRegisters,
  Index is 1 + ((RegisterShift + N) mod RegisterCount),
  arg(Index,RegisterNames,Name).

get_first_register(Register,State) :-
  state(UtilizedRegisters,_,_,_,_) = State,
  RegisterIndex is UtilizedRegisters,
  get_register(Register,RegisterIndex,State).

allocate_register(Register,
                  [],
                  state(UtilizedRegisters,
                        RegisterCount,
                        RegisterShift,
                        StackOffset,
                        RegisterNames),
                  state(NewUtilized,
                        RegisterCount,
                        RegisterShift,
                        StackOffset,
                        RegisterNames)) :-
  UtilizedRegisters < RegisterCount,
  !,
  get_first_register(
    Register,
    state(UtilizedRegisters,
          RegisterCount,
          RegisterShift,
          StackOffset,
          RegisterNames)),
  NewUtilized is UtilizedRegisters + 1.

allocate_register(RegisterToBuffer,
                  [push(RegisterToBuffer)],
                  state(UtilizedRegisters,
                        RegisterCount,
                        RegisterShift,
                        StackOffset,
                        RegisterNames),
                  state(UtilizedRegisters,
                        RegisterCount,
                        NewRegisterShift,
                        NewStackOffset,
                        RegisterNames)) :-
  UtilizedRegisters == RegisterCount,
  !,
  get_register(
    RegisterToBuffer,
    0,
    state(UtilizedRegisters,
          RegisterCount,
          RegisterShift,
          StackOffset,
          RegisterNames)),
  NewStackOffset is StackOffset + 1,
  NewRegisterShift is RegisterShift + 1.

nip(NipCount,
    [mov(TopOfstack,Destination)],
    state(UtilizedRegisters,
          RegisterCount,
          RegisterShift,
          StackOffset,
          RegisterNames),
    state(NewUtilization,
          RegisterCount,
          RegisterShift,
          StackOffset,
          RegisterNames)) :-
  UtilizedRegisters > 0,
  NipCount < UtilizedRegisters,
  !,
  Top is UtilizedRegisters - 1,
  Target is UtilizedRegisters - (NipCount+1),
  NewUtilization is UtilizedRegisters - NipCount,
  get_register(TopOfstack,
               Top,
               state(UtilizedRegisters,
                     RegisterCount,
                     RegisterShift,
                     StackOffset,
                     RegisterNames)),
  get_register(Destination,
               Target,
               state(UtilizedRegisters,
                     RegisterCount,
                     RegisterShift,
                     StackOffset,
                     RegisterNames)).

nip(NipCount,
    [add(StackNips * cell_size,stack_pointer)],
    state(UtilizedRegisters,
          RegisterCount,
          RegisterShift,
          StackOffset,
          RegisterNames),
    state(1,RegisterCount,NewRegisterShift,NewStackOffset,RegisterNames)) :-
  NipCount >= UtilizedRegisters,
  UtilizedRegisters > 0,
  !,
  %later
  %write(here),nl,
  RegisterNips is UtilizedRegisters-1,
  NewRegisterShift is RegisterShift + RegisterNips,
  StackNips is NipCount - RegisterNips,
  NewStackOffset is StackOffset - StackNips.

nip(NipCount,
    [
      pop(FirstRegister),
      add(NipCount*cell_size,stack_pointer)
    ],
    state(0,RegisterCount,RegisterShift,StackOffset,RegisterNames),
    %assume at least one registerget_first_register
    state(1,RegisterCount,RegisterShift,NewStackOffset,RegisterNames)) :-
  %StackNips is NipCount - 1,
  get_register(FirstRegister,
               0,
               state(0,
                     RegisterCount,
                     RegisterShift,
                     StackOffset,
                     RegisterNames)),
  NewStackOffset is StackOffset - (NipCount+1).
drop(0,[],S,S) :- !.
drop(N,
     [add(N*cell_size,stack_pointer)],
     state(0,
           RegisterCount,
           RegisterShift,
           StackOffset,
           RegisterNames),
     state(0,
           RegisterCount,
           RegisterShift,
           NewStackOffset,
           RegisterNames)) :-
  !,
  NewStackOffset is StackOffset - N.

drop(N,
     [],
     state(UtilizedRegisters,
           RegisterCount,
           RegisterShift,
           StackOffset,
           RegisterNames),
     state(NewUtilization,
           RegisterCount,
           RegisterShift,
           StackOffset,
           RegisterNames)) :-
  UtilizedRegisters >= N,
  !,
  NewUtilization is UtilizedRegisters - N.

drop(N,
     [add(StackRemoves*cell_size,stack_pointer)],
     state(UtilizedRegisters,
           RegisterCount,
           RegisterShift,
           StackOffset,
           RegisterNames),
     state(0,
           RegisterCount,
           RegisterShift,
           NewStackOffset,
           RegisterNames)) :-
  UtilizedRegisters < N,
  !,
  StackRemoves is N - UtilizedRegisters,
  NewStackOffset is StackOffset - StackRemoves.

remove_all_but(NumberToPreserve,
               Code,
               state(UtilizedRegisters,
                     RegisterCount,
                     RegisterShift,
                     StackOffset,
                     RegisterNames),
               NewState) :-
  NumberToRemove is (UtilizedRegisters + StackOffset) - NumberToPreserve,
  drop(NumberToRemove,
         Code,
         state(UtilizedRegisters,
               RegisterCount,
               RegisterShift,
               StackOffset,
               RegisterNames),
         NewState).

pick(stack(StackIndex),Position,state(UtilizedRegisters,_,_,_,_)) :-
  Position >= UtilizedRegisters,
  !,
  StackIndex is Position - UtilizedRegisters.
pick(Place,
     Position,
     state(UtilizedRegisters,
           RegisterCount,
           RegisterShift,
           StackOffset,
           RegisterNames)) :-
  RegisterIndex is UtilizedRegisters - (1 + Position),
  get_register(
    Place,
    RegisterIndex,
    state(UtilizedRegisters,
          RegisterCount,
          RegisterShift,
          StackOffset,
          RegisterNames)).
  
pick_from_top(stack(StackIndex),
              Index,
              state(_UtilizedRegisters,
                    _RegisterCount,
                    _RegisterShift,
                    StackOffset,
                    _RegisterNames)) :-
  Index < StackOffset,
  !,
  StackIndex = StackOffset-(Index+1).
pick_from_top(reg(Name),
              Index,
              state(UtilizedRegisters,
                    RegisterCount,
                    RegisterShift,
                    StackOffset,
                    RegisterNames)) :-
  Index >= StackOffset,
  !,
  RegisterIndex = (Index-StackOffset),
  PickIndex = UtilizedRegisters - (RegisterIndex + 1),
  pick(reg(Name),PickIndex,
  state(UtilizedRegisters,
        RegisterCount,
        RegisterShift,
        StackOffset,
        RegisterNames)).

move_n_to_top(Count,
              [],
              state(UtilizedRegisters,
                    RegisterCount,
                    RegisterShift,
                    StackOffset,
                    RegisterNames),
              state(UtilizedRegisters,
                    RegisterCount,
                    RegisterShift,
                    StackOffset,
                    RegisterNames)) :-
  Count is UtilizedRegisters + StackOffset,
  !.
move_n_to_top(N,Code,State,NewState) :-
  move_n_to_top(N,N,Code,State,NewState).

move_n_to_top(0,_,[],State,State) :- !.

move_n_to_top(Count,StartCount,Code,State,NewState) :-
  Location is Count-1,
  pick(reg(R),Location,State),
  !,
  TargetLocation is StartCount - Count,
  pick_from_top(Target,TargetLocation,State),
  move_n_to_top(Location,StartCount,RestCode,State,NewState),
  appendAll([[mov(reg(R),Target)],RestCode],Code).
  
move_n_to_top(Count,StartCount,Code,State,NewState) :-
  Location is Count-1,
  pick(stack(S),Location,State),
  !,
  TargetLocation is StartCount - Count,
  pick_from_top(Target,TargetLocation,State),
  move_n_to_top(Location,StartCount,RestCode,State,NewState),
  appendAll(
    [
      [
        mov(stack(S),
        reg(rax)),
        mov(reg(rax),Target)
      ],
      RestCode
    ],
    Code).

forth_to_asm(A,B,C,D,E) :-
  ifVerbose((nl,write(compiling -> (A,B,C,D,E)),nl)),
  fail.

forth_to_asm([],
             [ret],
             state(_,RegisterCount,RegisterShift,0,RegisterNames),
             state(0,RegisterCount,RegisterShift,0,RegisterNames),
             0) :-
  !.

forth_to_asm([],
             [
               add(N*cell_size,stack_pointer),
               ret
             ],
             state(_,RegisterCount,RegisterShift,N,RegisterNames),
             state(0,RegisterCount,RegisterShift,0,RegisterNames),
             0) :-
  N > 0,
  !.

% no registers used and StackOffset is one
forth_to_asm([],
             [
               pop(reg(rax)),
               ret
             ],
             state(0,RegisterCount,RegisterShift,1,RegisterNames),
             state(0,RegisterCount,RegisterShift,0,RegisterNames),
             1) :-
  !.

% no registers used and StackOffset is nonzero
forth_to_asm([],
             [
               pop(reg(rax)),
               add(Extra*cell_size,stack_pointer),
               ret
             ],
             state(0,RegisterCount,RegisterShift,StackOffset,RegisterNames),
             state(0,RegisterCount,RegisterShift,0,RegisterNames),
             1) :-
  StackOffset > 0,
  !,
  Extra is StackOffset - 1.

% registers are used and StackOffset is 0
forth_to_asm([],
             [mov(Result,reg(rax)),ret],
             state(UtilizedRegisters,
                   RegisterCount,
                   RegisterShift,
                   0,
                   RegisterNames),
             state(0,
                   RegisterCount,
                   RegisterShift,
                   0,
                   RegisterNames),
             1) :-
  UtilizedRegisters > 0,
  !,
  pick(Result,
       0,
       state(UtilizedRegisters,
             RegisterCount,
             RegisterShift,
             0,
             RegisterNames)).

% registers are used and StackOffset is nonzero
forth_to_asm([],
             [
               mov(Result,reg(rax)),
               add(StackOffset*cell_size,stack_pointer),
               ret
             ],
             state(UtilizedRegisters,
                   RegisterCount,
                   RegisterShift,
                   StackOffset,
                   RegisterNames),
             state(0,
                   RegisterCount,
                   RegisterShift,
                   0,
                   RegisterNames),
             1) :-
  StackOffset > 0,
  !,
  pick(Result,
       0,
       state(UtilizedRegisters,
             RegisterCount,
             RegisterShift,
             StackOffset,
             RegisterNames)).

% no registers used and StackOffset is 0 means the function failed

forth_to_asm(
  [
    num(int(ReturnLabelName)),
    func(Name,ArgTypes,ArgCount,0,label(ReturnLabelName))
    |
    Rest
  ],
  Code,
  state(0,RegisterCount,RegisterShift,StackOffset,RegisterNames),
  NewState,
  RC) :-

  !,
  cleanLabel((Name,ArgTypes),FunctionName),
  PrefixCode = 
    [
      push('$'(ReturnLabelName)),
      jmp(label(FunctionName)),
      label(ReturnLabelName)
    ],
  % + label
  % - label
  % - ArgCount
  % + push(%eax)
  NewStackOffset is StackOffset - ArgCount,
  StateAfterCall =
    state(0,RegisterCount,RegisterShift,NewStackOffset,RegisterNames),
  forth_to_asm(Rest,RestCode,StateAfterCall,NewState,RC),
  !,
  append(PrefixCode,RestCode,Code).

forth_to_asm(
  [
    num(int(ReturnLabelName)),
    func(Name,ArgTypes,ArgCount,FRC,label(ReturnLabelName))
    |
    Rest
  ],
  Code,
  state(0,RegisterCount,RegisterShift,StackOffset,RegisterNames),
  NewState,
  RC) :-

  !,
  cleanLabel((Name,ArgTypes),FunctionName),
  PrefixCode = 
    [
      push('$'(ReturnLabelName)),
      jmp(label(FunctionName)),
      label(ReturnLabelName),
      push(reg(rax))
    ],
  % + label
  % - label
  % - ArgCount
  % + push(%eax)
  NewStackOffset is (StackOffset - ArgCount)+FRC,
  StateAfterCall =
    state(0,RegisterCount,RegisterShift,NewStackOffset,RegisterNames),
  forth_to_asm(Rest,RestCode,StateAfterCall,NewState,RC),
  !,
  append(PrefixCode,RestCode,Code).

forth_to_asm([exit|Rest],
             Code,
             State,
             NewState,
             RC) :-
  forth_to_asm([],ExitCode,State,_State1,RC),
  forth_to_asm(Rest,RestCode,State,NewState,RC),
  append(ExitCode,RestCode,Code).
forth_to_asm([if(label(FailLabel),State1)|Rest],
             Code,
             State,
             NewState,
             RC) :-
  !,
  pick(TopOfStack,0,State),
  drop(1,Dropping,State,State1),
  appendAll(
    [
      [
        mov(TopOfStack,reg(rax)),
        test(reg(rax),reg(rax))
      ],
      Dropping,
      [
        je(label(FailLabel))
      ]
    ],
    PrefixCode),
  forth_to_asm(Rest,RestCode,State1,NewState,RC),
  append(PrefixCode,RestCode,Code).

forth_to_asm([drop,drop|Rest],
             Code,
             State,
             NewState,
             RC) :-
  !,
  forth_to_asm([drop(2)|Rest],
               Code,
               State,
               NewState,
               RC).

forth_to_asm([drop(N),drop|Rest],
             Code,
             State,
             NewState,
             RC) :-
  !,
  NewCount is N+1,
  forth_to_asm([drop(NewCount)|Rest],
               Code,
               State,
               NewState,
               RC).

forth_to_asm([drop(N),drop(N2)|Rest],
             Code,
             State,
             NewState,
             RC) :-
  !,
  NewCount is N+N2,
  forth_to_asm([drop(NewCount)|Rest],
               Code,
               State,
               NewState,
               RC).

forth_to_asm([drop(N)|Rest],
             Code,
             State,
             NewState,
             RC) :-
  !,
  drop(N,Dropping,State,State1),
  forth_to_asm(Rest,RestCode,State1,NewState,RC),
  append(Dropping,RestCode,Code).

forth_to_asm([drop|Rest],
             Code,
             State,
             NewState,
             RC) :-
  !,
  drop(1,Dropping,State,State1),
  forth_to_asm(Rest,RestCode,State1,NewState,RC),
  append(Dropping,RestCode,Code).

forth_to_asm([then(AfterLabel,State1) | Rest],
             [AfterLabel | RestCode],
             _State,
             NewState,
             RC) :-
  !,
  forth_to_asm(Rest,RestCode,State1,NewState,RC).

%tailcall at end of function
forth_to_asm([tailcall(Name,ArgTypes,ArgCount,_TRC)],
             Code,
             State,
             NewState,
             _RC) :-
  !,
  move_n_to_top(ArgCount,Moving,State,State1),
  remove_all_but(ArgCount,Dropping,State1,State2),
  push_all_registers(RegisterPushingCode,State2,NewState),
  cleanLabel((Name,ArgTypes),FunctionName),
  appendAll(
    [
      Moving,
      Dropping,
      RegisterPushingCode,
      [jmp(label(FunctionName))]
    ],
    Code).

forth_to_asm([tailcall(Name,ArgTypes,ArgCount,_TRC)|Rest],
             Code,
             State,
             NewState,
             RC) :-
  move_n_to_top(ArgCount,Moving,State,State1),
  remove_all_but(ArgCount,Dropping,State1,State2),
  push_all_registers(RegisterPushingCode,State2,State3),
  cleanLabel((Name,ArgTypes),CleanName),
  appendAll(
    [
      Moving,
      Dropping,
      RegisterPushingCode,
      [jmp(CleanName)]
    ],
    PrefixCode),
  forth_to_asm(Rest,RestCode,State3,NewState,RC),
  append(PrefixCode,RestCode,Code).

forth_to_asm([
               func(Name,
                    ArgTypes,
                    ArgCount,
                    FRC,
                    label(ReturnLabelName))
               | Rest
             ],
             Code,
             State,
             NewState,
             RC) :-
  push_all_registers(RegisterPushingCode,State,StateAfterPush),
  !,
  cleanLabel((Name,ArgTypes),CleanName),
  append(RegisterPushingCode,
         [
           jmp(label(CleanName)),
           label(ReturnLabelName)
         ],
         PrefixCode1),
  (FRC =:= 1 ->
    append(PrefixCode1,[push(reg(rax))], PrefixCode)
    ;
    PrefixCode = PrefixCode1),
  StateAfterPush =
    state(0,RegisterCount,RegisterShift,StackOffset,RegisterNames),
  % - Label
  % - ArgCount
  % + push(%eax)
  NewStackOffset is (FRC - 1) + (StackOffset - ArgCount),
  StateAfterCall =
    state(0,RegisterCount,RegisterShift,NewStackOffset,RegisterNames),
  forth_to_asm(Rest,RestCode,StateAfterCall,NewState,RC),
  !,
  append(PrefixCode,RestCode,Code).

forth_to_asm([over|Rest],Code,S,NewS,RC) :-
  !,
  forth_to_asm([1,pick|Rest],Code,S,NewS,RC).

forth_to_asm([dup|Rest],Code,S,NewS,RC) :-
  !,
  pick(TopOfStack,0,S),
  allocate_register(Target,Buffering,S,S1),
  forth_to_asm(Rest,RestCode,S1,NewS,RC),
  appendAll([Buffering,[mov(TopOfStack,Target)],RestCode], Code).

forth_to_asm(['2dup'|Rest],Code,S,NewS,RC) :-
  !,
  allocate_register(FirstRegister,Buffering,S,S1),
  allocate_register(SecondRegister,Buffering1,S1,S2),
  pick(First,3,S2),
  pick(Second,2,S2),
  forth_to_asm(Rest,RestCode,S2,NewS,RC),
  appendAll(
    [
      Buffering,
      Buffering1,
      [
        mov(First,FirstRegister),
        mov(Second,SecondRegister)
      ],
      RestCode
    ],
    Code).

%forth_to_asm([execute(N)|Rest],Code,S,NewS) :-
%forth_to_asm([call(function,N)|Rest],Code,S,NewS) :-

forth_to_asm([N,pick|Rest],
             [mov(Location,Target)|RestCode],
             S,
             NewS,
             RC) :-
  number(N),
  pick(Location,N,S),
  % no buffering
  allocate_register(Target,[],S,S1),
  !,
  forth_to_asm(Rest,RestCode,S1,NewS,RC).

forth_to_asm([N,pick|Rest],Code,S,NewS,RC) :-
  number(N),
  allocate_register(Target,Buffering,S,S1),
  NewN is N + 1,
  pick(Location,NewN,S),
  forth_to_asm(Rest,RestCode,S1,NewS,RC),
  appendAll([Buffering,[mov(Location,Target)],RestCode],Code).

forth_to_asm([swap|Rest],Code,S,NS,RC) :-
  S = state(UtilizedRegisters,_,_,_,_),
  UtilizedRegisters > 0,
  !,
  pick(LocationA,0,S),
  pick(LocationB,1,S),
  forth_to_asm(Rest,RestCode,S,NS,RC),
  append([xchg(LocationA,LocationB)],RestCode,Code).

forth_to_asm([swap | Rest],
             Code,
             state(0,
                   RegisterCount,
                   RegisterShift,
                   StackOffset,
                   RegisterNames),
             NewState,
             RC) :-
  !,
  get_first_register(
    Register,
    state(0,RegisterCount,RegisterShift,StackOffset,RegisterNames)),
  StackOffset1 is StackOffset - 1,
  State1 = state(1,RegisterCount,RegisterShift,StackOffset1,RegisterNames),
  forth_to_asm([swap|Rest],RestCode,State1,NewState,RC),
  append([pop(Register)],RestCode,Code).

forth_to_asm([intrinsic(Name,[Type1],1) | Rest],
             Code,
             State,
             NewState,
             RC) :-
  !,
  intrinsic_instructions(intrinsic(Name,[Type1],1),Target,OperationCode),
  force_to_register_and_get_first(Target,Buffering,State,State1),
  forth_to_asm(Rest,RestCode,State1,NewState,RC),
  appendAll([Buffering,OperationCode,RestCode],Code).

forth_to_asm(
  [intrinsic(Name,[Type1,Type2],1) | Rest],
  Code,
  State,
  NewState,
  RC) :-

  intrinsic(Name,[Type1,Type2],_ReturnType,1),
  !,
  intrinsic_instructions(
    intrinsic(Name,[Type1,Type2],1),
    Source,
    Destination,
    OperationCode),
  force_to_register_and_get_first(
    Source,
    Buffering,
    State,
    state(UtilizedRegisters,
          RegisterCount,
          RegisterShift,
          StackOffset,
          RegisterNames)),
  pick(Destination,
       1,
       state(UtilizedRegisters,
             RegisterCount,
             RegisterShift,
             StackOffset,
             RegisterNames)),
  NewUtilization is UtilizedRegisters - 1,
  State1 =
    state(NewUtilization,
          RegisterCount,
          RegisterShift,
          StackOffset,
          RegisterNames),
  forth_to_asm(Rest,RestCode,State1,NewState,RC),
  appendAll([Buffering,OperationCode,RestCode],Code).

%forth_to_asm([label(Name)|Rest],[label(Name)|RestCode],State,NewState) :-
  %forth_to_asm(Rest,RestCode,State,NewState).

forth_to_asm([num(int(N)) | Rest],Result,State,NewState,RC) :-
  !,
  allocate_register(Register,Buffer,State,State1),
  forth_to_asm(Rest,RestCode,State1,NewState,RC),
  appendAll([Buffer,[mov('$'(N),Register)],RestCode],Result).

forth_to_asm([num(float(N))|Rest],Result,State,NewState,RC) :-
  !,
  allocate_register(Register,Buffer,State,State1),
  forth_to_asm(Rest,RestCode,State1,NewState,RC),
  appendAll([Buffer,[mov('$'(N),Register)],RestCode],Result).

forth_to_asm([nip,nip|Rest],Code,R,NewR,RC) :-
  !,
  forth_to_asm([nip(2)|Rest],Code,R,NewR,RC).

forth_to_asm([nip|Rest],Code,R,NewR,RC) :-
  !,
  forth_to_asm([nip(1)|Rest],Code,R,NewR,RC).

forth_to_asm([nip(N),nip|Rest],Code,R,NewR,RC) :-
  NewN is N + 1,
  !,
  forth_to_asm([nip(NewN)|Rest],Code,R,NewR,RC).

forth_to_asm([nip(N)|Rest],Code,R,NewR,RC) :-
  !,
  nip(N,StartCode,R,R1),
  forth_to_asm(Rest,RestCode,R1,NewR,RC),
  append(StartCode,RestCode,Code).
  % TODO fix backtracking craziness if still exists
  %write(append(StartCode,RestCode,Code)),nl.
pretty_instructions([]) :- nl,nl.
pretty_instructions([A|R]) :-
  ifVerbose(format("    ~w~n",[A])),
  pretty_instructions(R).

psuedo_asm_to_x64([],[]) :- !.
psuedo_asm_to_x64([First|Rest],[FirstString|RestStrings]) :-
  translate_instruction(First,FirstString),
  !,
  psuedo_asm_to_x64(Rest,RestStrings).

forth_to_x86(ArgCount,Forth,Assembly,ReturnCount) :-
  start_state(ArgCount,Start),
  forth_to_asm(Forth,PseudoAssembly,Start,_EndState,ReturnCount), !,
  ifVerbose((write(PseudoAssembly), nl)),
  ifVerbose(format("~n    Assembly Optimizations:~n~n")),
  assembly_optimize(20,PseudoAssembly,OptimizedPseudoAssembly), !,
  ifVerbose(format("~n    Done Optimizing~n~n")),
  (psuedo_asm_to_x64(OptimizedPseudoAssembly,Assembly) ->
    ifVerbose(format("~n    Done Translating PseudoAssembly~n~n"))
    ;
    format("~n    Unable to Translate PseudoAssembly~n~n"),
    fail).
