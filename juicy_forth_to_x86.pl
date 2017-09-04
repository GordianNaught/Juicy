:- module(juicy_forth_to_x86, [forth_to_x86/3]).

:- use_module(utils).
:- use_module(library(clpfd)).
:- use_module(assembly_optimize, [assembly_optimize/3]).
:- use_module(juicy_intrinsics).
:- use_module(juicy_global).

size_of(int,8).
size_of(float,8).
size_of(byte,1).
size_of(bool,1).
size_of(char,4).

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

%state(UtilizedRegisters,RegisterCount,RegisterShift,StackOffset,RegisterNames).
start_state(ArgCount,state(0,8,0,ArgCount,registers(r8,r9,r10,r11,r12,r13,r14,r15))).


op_instruction(Op,_Source,_Destination,_) :-
  !,
  format("Unable to find op: ~w~n",[Op]),
  fail.

op_instruction(op(-,int),Target,[neg(Target)]) :- !.

op_instruction(Op,_Target,_) :-
  !,
  format("Unable to find op: ~w~n",[Op]).

translate_location(reg(Name),String) :-
  !,
  format(string(String),"%~w",[Name]).

translate_location(stack(N),String) :-
  !,
  Offset is N*8,
  format(string(String),"~w(%rsp)",[Offset]).

translate_location(N*cell_size,String) :-
  !,
  Number is N * 8,
  format(string(String),"$~w",Number).

translate_location('$'(N),String) :-
  !,
  format(string(String),"$~w",[N]).
translate_location(label(Name), Name).
translate_location(stack_pointer,"%rsp").
translate_location(Atom, String) :-
  atom(Atom),
  !,
  format(string(String), "~w", Atom).

translate_instruction_name(call,"call").
translate_instruction_name(fst,"fst").
translate_instruction_name(fadd,"fadd").
translate_instruction_name(fmul,"fmul").
translate_instruction_name(fsub,"fst").

translate_instruction_name(xorl,"xorl").
translate_instruction_name(cmp,"cmpq").
translate_instruction_name(setge,"setge").
translate_instruction_name(setne,"setne").
translate_instruction_name(setgt,"setg").
translate_instruction_name(setlt,"setl").
translate_instruction_name(setle,"setle").
translate_instruction_name(sete,"sete").
translate_instruction_name(neg,"negq").
translate_instruction_name(test,"test").

translate_instruction_name(movsbq,"movsbq").
translate_instruction_name(jng,"jng").
translate_instruction_name(jne,"jne").
translate_instruction_name(je,"je").
translate_instruction_name(imul,"imulq").
translate_instruction_name(xchg,"xchgq").
translate_instruction_name(jmp,"jmp").
translate_instruction_name(push,"pushq").
translate_instruction_name(pop,"popq").
translate_instruction_name(add,"addq").
translate_instruction_name(sub,"subq").
translate_instruction_name(idiv,"idivq").
translate_instruction_name(mov,"movq").
translate_instruction_name(movsbl,"movsbl").
translate_instruction_name(xorl,"xorl").
translate_instruction_name(xor,"xorq").
translate_instruction_name(sarl,"sarlq").
translate_instruction_name(sarr,"sarrq").
translate_instruction_name(Any,_) :-
  format("Unable to find instruction name for: ~w~n",[Any]),
  fail.
translate_instruction(ret,"ret") :- !.
translate_instruction(label(Name),String) :-
  !,
  format(string(String),"~w:",[Name]).

translate_instruction(Instruction,String) :-
  Instruction =.. [InstructionName, Location1, Location2],
  !,
  translate_instruction_name(InstructionName, InstructionNameString),
  !,
  translate_location(Location1, Location1String),
  translate_location(Location2, Location2String),
  format(string(String),"~w   ~w, ~w",[InstructionNameString, Location1String, Location2String]).

translate_instruction(Instruction,String) :-
  Instruction =.. [InstructionName, Location],
  !,
  translate_instruction_name(InstructionName, InstructionNameString),
  !,
  translate_location(Location, LocationString),
  format(string(String),"~w   ~w",[InstructionNameString, LocationString]).
  
translate_instruction(reg(Name),String) :-
  !,
  format(string(String),"popq(%~w)",[Name]).

translate_instruction(NotFound,_) :- format("cannot translate assembly instruction: ~w~n",[NotFound]), !, fail.

push_all_registers([],
                   state(0,RegisterCount,RegisterShift,StackOffset,RegisterNames),
                   state(0,RegisterCount,RegisterShift,StackOffset,RegisterNames)) :-
  !.
             
push_all_registers([push(Register) | RestInstructions],
                   state(UtilizedRegisters,RegisterCount,RegisterShift,StackOffset,RegisterNames),
                   NewState) :-
  RegisterLocation is UtilizedRegisters - 1,
  pick(Register,RegisterLocation,state(UtilizedRegisters,RegisterCount,RegisterShift,StackOffset,RegisterNames)),
  RegistersLeft = RegisterLocation,
  NewOffset is StackOffset + 1,
  NewRegisterShift is RegisterShift + 1,
  !,
  push_all_registers(RestInstructions,state(RegistersLeft,RegisterCount,NewRegisterShift,NewOffset,RegisterNames),NewState).

force_to_register_and_get_first(Register,
                                [pop(Register)],
                                state(0,RegisterCount,RegisterShift,StackOffset,RegisterNames),
                                state(1,RegisterCount,RegisterShift,NewStackOffset,RegisterNames)) :-
  !,
  get_first_register(Register,state(0,RegisterCount,RegisterShift,StackOffset,RegisterNames)),
  NewStackOffset is StackOffset - 1.
force_to_register_and_get_first(Register,[],State,State) :-
  !, pick(Register,0,State).


get_register(reg(Name),N,state(UtilizedRegisters,RegisterCount,RegisterShift,_,RegisterNames)) :-
  N =< UtilizedRegisters,
  Index is 1 + ((RegisterShift + N) mod RegisterCount),
  arg(Index,RegisterNames,Name).

get_first_register(Register,State) :-
  state(UtilizedRegisters,_,_,_,_) = State,
  RegisterIndex is UtilizedRegisters,
  get_register(Register,RegisterIndex,State).

allocate_register(Register,
                  [],
                  state(UtilizedRegisters,RegisterCount,RegisterShift,StackOffset,RegisterNames),
                  state(NewUtilized,RegisterCount,RegisterShift,StackOffset,RegisterNames)) :-
  UtilizedRegisters < RegisterCount,
  !,
  get_first_register(Register,state(UtilizedRegisters,RegisterCount,RegisterShift,StackOffset,RegisterNames)),
  NewUtilized is UtilizedRegisters + 1.
  
allocate_register(RegisterToBuffer,
                  [push(RegisterToBuffer)],
                  state(UtilizedRegisters,RegisterCount,RegisterShift,StackOffset,RegisterNames),
                  state(UtilizedRegisters,RegisterCount,NewRegisterShift,NewStackOffset,RegisterNames)) :-
  UtilizedRegisters == RegisterCount,
  !,
  get_register(RegisterToBuffer,0,state(UtilizedRegisters,RegisterCount,RegisterShift,StackOffset,RegisterNames)),
  NewStackOffset is StackOffset + 1,
  NewRegisterShift is RegisterShift + 1.

nip(NipCount,
    [mov(TopOfstack,Destination)],
    state(UtilizedRegisters,RegisterCount,RegisterShift,StackOffset,RegisterNames),
    state(NewUtilization,RegisterCount,RegisterShift,StackOffset,RegisterNames)) :-
  UtilizedRegisters > 0,
  NipCount < UtilizedRegisters,
  !,
  Top is UtilizedRegisters - 1,
  Target is UtilizedRegisters - (NipCount+1),
  NewUtilization is UtilizedRegisters - NipCount,
  get_register(TopOfstack,Top,state(UtilizedRegisters,RegisterCount,RegisterShift,StackOffset,RegisterNames)),
  get_register(Destination,Target,state(UtilizedRegisters,RegisterCount,RegisterShift,StackOffset,RegisterNames)).

nip(NipCount,
    [add(StackNips * cell_size,stack_pointer)],
    state(UtilizedRegisters,RegisterCount,RegisterShift,StackOffset,RegisterNames),
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
    [pop(FirstRegister),add(NipCount*cell_size,stack_pointer)],
    state(0,RegisterCount,RegisterShift,StackOffset,RegisterNames),
    %assume at least one registerget_first_register
    state(1,RegisterCount,RegisterShift,NewStackOffset,RegisterNames)) :-
  %StackNips is NipCount - 1,
  get_register(FirstRegister,0,state(0,RegisterCount,RegisterShift,StackOffset,RegisterNames)),
  NewStackOffset is StackOffset - (NipCount+1).
drop(0,[],S,S) :- !.
drop(N,
     [add(N*cell_size,stack_pointer)],
     state(0,RegisterCount,RegisterShift,StackOffset,RegisterNames),
     state(0,RegisterCount,RegisterShift,NewStackOffset,RegisterNames)) :-
  !,
  NewStackOffset is StackOffset - N.

drop(N,
     [],
     state(UtilizedRegisters,RegisterCount,RegisterShift,StackOffset,RegisterNames),
     state(NewUtilization,RegisterCount,RegisterShift,StackOffset,RegisterNames)) :-
  UtilizedRegisters >= N,
  !,
  NewUtilization is UtilizedRegisters - N.

drop(N,
     [add(StackRemoves*cell_size,stack_pointer)],
     state(UtilizedRegisters,RegisterCount,RegisterShift,StackOffset,RegisterNames),
     state(0,RegisterCount,RegisterShift,StackOffset,RegisterNames)) :-
  UtilizedRegisters < N,
  !,
  StackRemoves is N - UtilizedRegisters.

remove_all_but(NumberToPreserve,
               Code,
               state(UtilizedRegisters,RegisterCount,RegisterShift,StackOffset,RegisterNames),
               NewState) :-
  NumberToRemove is (UtilizedRegisters + StackOffset) - NumberToPreserve,
  drop(NumberToRemove,
         Code,
         state(UtilizedRegisters,RegisterCount,RegisterShift,StackOffset,RegisterNames),
         NewState).
  

pick(stack(StackIndex),Position,state(UtilizedRegisters,_,_,_,_)) :-
  Position >= UtilizedRegisters,
  !,
  StackIndex is Position - UtilizedRegisters.
pick(Place,Position,state(UtilizedRegisters,RegisterCount,RegisterShift,StackOffset,RegisterNames)) :-
  RegisterIndex is UtilizedRegisters - (1 + Position),
  get_register(Place,RegisterIndex,state(UtilizedRegisters,RegisterCount,RegisterShift,StackOffset,RegisterNames)).
  
pick_from_top(stack(StackIndex),Index,state(_UtilizedRegisters,_RegisterCount,_RegisterShift,StackOffset,_RegisterNames)) :-
  Index < StackOffset,
  !,
  StackIndex = StackOffset-(Index+1).
pick_from_top(reg(Name),Index,state(UtilizedRegisters,RegisterCount,RegisterShift,StackOffset,RegisterNames)) :-
  Index >= StackOffset,
  !,
  RegisterIndex = (Index-StackOffset),
  PickIndex = UtilizedRegisters - (RegisterIndex + 1),
  pick(reg(Name),PickIndex,
  state(UtilizedRegisters,RegisterCount,RegisterShift,StackOffset,RegisterNames)).

move_n_to_top(Count,
              [],
              state(UtilizedRegisters,RegisterCount,RegisterShift,StackOffset,RegisterNames),
              state(UtilizedRegisters,RegisterCount,RegisterShift,StackOffset,RegisterNames)) :-
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
  appendAll([[mov(stack(S),reg(rax)),mov(reg(rax),Target)],RestCode],Code).
  
forth_to_asm(A,B,C,D) :-
  ifVerbose((write((A,B,C,D)),nl)),
  fail.

% no registers used and StackOffset is one
forth_to_asm([],
             [pop(reg(rax)),ret],
             state(0,RegisterCount,RegisterShift,1,RegisterNames),
             state(0,RegisterCount,RegisterShift,0,RegisterNames)) :-
  !.

% no registers used and StackOffset is nonzero
forth_to_asm([],
             [pop(reg(rax)),add(Extra*cell_size,stack_pointer),ret],
             state(0,RegisterCount,RegisterShift,StackOffset,RegisterNames),
             state(0,RegisterCount,RegisterShift,0,RegisterNames)) :-
  StackOffset > 0,
  !,
  Extra is StackOffset - 1.

% registers are used and StackOffset is 0
forth_to_asm([],
             [mov(Result,reg(rax)),ret],
             state(UtilizedRegisters,RegisterCount,RegisterShift,0,RegisterNames),
             state(0,RegisterCount,RegisterShift,0,RegisterNames)) :-
  UtilizedRegisters > 0,
  !,
  pick(Result,0,state(UtilizedRegisters,RegisterCount,RegisterShift,0,RegisterNames)).

% registers are used and StackOffset is nonzero
forth_to_asm([],
             [mov(Result,reg(rax)),add(StackOffset*cell_size,stack_pointer),ret],
             state(UtilizedRegisters,RegisterCount,RegisterShift,StackOffset,RegisterNames),
             state(0,RegisterCount,RegisterShift,0,RegisterNames)) :-
  StackOffset > 0,
  !,
  pick(Result,0,state(UtilizedRegisters,RegisterCount,RegisterShift,StackOffset,RegisterNames)).

% no registers used and StackOffset is 0 means the function failed

forth_to_asm([num(int(ReturnLabelName)),func(Name,ArgTypes,ArgCount,label(ReturnLabelName))|Rest],
             Code,
             state(0,RegisterCount,RegisterShift,StackOffset,RegisterNames),
             NewState) :-
  !,
  cleanLabel((Name,ArgTypes),FunctionName),
  PrefixCode = 
    [
      push('$'(ReturnLabelName)),
      jmp('$'(FunctionName)),
      label(ReturnLabelName),
      push(reg(rax))
    ],
  % + label
  % - label
  % - ArgCount
  % + push(%eax)
  NewStackOffset is (StackOffset - ArgCount)+1,
  StateAfterCall = state(0,RegisterCount,RegisterShift,NewStackOffset,RegisterNames),
  forth_to_asm(Rest,RestCode,StateAfterCall,NewState),
  !,
  append(PrefixCode,RestCode,Code).

forth_to_asm([exit|Rest],
             Code,
             State,
             NewState) :-
  forth_to_asm([],ExitCode,State,_State1),
  forth_to_asm(Rest,RestCode,State,NewState),
  append(ExitCode,RestCode,Code).
forth_to_asm([if(label(FailLabel),State1)|Rest],
             Code,
             State,
             NewState) :-
  !,
  pick(TopOfStack,0,State),
  drop(1,Dropping,State,State1),
  appendAll([[mov(TopOfStack,reg(rax)),test(reg(rax),reg(rax))],Dropping,[je(label(FailLabel))]], PrefixCode),
  forth_to_asm(Rest,RestCode,State1,NewState),
  append(PrefixCode,RestCode,Code).

forth_to_asm([drop|Rest],
             Code,
             State,
             NewState) :-
  !,
  drop(1,Dropping,State,State1),
  forth_to_asm(Rest,RestCode,State1,NewState),
  append(Dropping,RestCode,Code).

forth_to_asm([then(AfterLabel,State1) | Rest],
             [AfterLabel | RestCode],
             _State,
             NewState) :-
  !,
  forth_to_asm(Rest,RestCode,State1,NewState).
  
%tailcall at end of function
forth_to_asm([tailcall(Name,ArgTypes,ArgCount)],
             Code,
             State,
             NewState) :-
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
             
forth_to_asm([tailcall(Name,ArgTypes,ArgCount)|Rest],
             Code,
             State,
             NewState) :-
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
  forth_to_asm(Rest,RestCode,State3,NewState),
  append(PrefixCode,RestCode,Code).

forth_to_asm([func(Name,ArgTypes,ArgCount,label(ReturnLabelName))|Rest],
             Code,
             State,
             NewState) :-
  push_all_registers(RegisterPushingCode,State,StateAfterPush),
  !,
  cleanLabel((Name,ArgTypes),CleanName),
  append(RegisterPushingCode,[jmp(label(CleanName)),label(ReturnLabelName),push(reg(rax))],PrefixCode),
  state(0,RegisterCount,RegisterShift,StackOffset,RegisterNames) = StateAfterPush,
  % - Label
  % - ArgCount
  % + push(%eax)
  NewStackOffset is StackOffset - ArgCount,
  StateAfterCall = state(0,RegisterCount,RegisterShift,NewStackOffset,RegisterNames),
  forth_to_asm(Rest,RestCode,StateAfterCall,NewState),
  !,
  append(PrefixCode,RestCode,Code).
  
forth_to_asm([over|Rest],Code,S,NewS) :-
  !,
  forth_to_asm([1,pick|Rest],Code,S,NewS).

forth_to_asm([dup|Rest],Code,S,NewS) :-
  !,
  pick(TopOfStack,0,S),
  allocate_register(Target,Buffering,S,S1),
  forth_to_asm(Rest,RestCode,S1,NewS),
  appendAll([Buffering,[mov(TopOfStack,Target)],RestCode], Code).

forth_to_asm(['2dup'|Rest],Code,S,NewS) :-
  !,
  allocate_register(FirstRegister,Buffering,S,S1),
  allocate_register(SecondRegister,Buffering1,S1,S2),
  pick(First,3,S2),
  pick(Second,2,S2),
  forth_to_asm(Rest,RestCode,S2,NewS),
  appendAll([Buffering,Buffering1,[mov(First,FirstRegister),mov(Second,SecondRegister)],RestCode],Code).
  
%forth_to_asm([execute(N)|Rest],Code,S,NewS) :-
%forth_to_asm([call(function,N)|Rest],Code,S,NewS) :-
  
forth_to_asm([N,pick|Rest],[mov(Location,Target)|RestCode],S,NewS) :-
  number(N),
  pick(Location,N,S),
  % no buffering
  allocate_register(Target,[],S,S1),
  !,
  forth_to_asm(Rest,RestCode,S1,NewS).
  
forth_to_asm([N,pick|Rest],Code,S,NewS) :-
  number(N),
  allocate_register(Target,Buffering,S,S1),
  NewN is N + 1,
  pick(Location,NewN,S),
  forth_to_asm(Rest,RestCode,S1,NewS),
  appendAll([Buffering,[mov(Location,Target)],RestCode],Code).

forth_to_asm([swap|Rest],Code,S,NS) :-
  S = state(UtilizedRegisters,_,_,_,_),
  UtilizedRegisters > 0,
  !,
  pick(LocationA,0,S),
  pick(LocationB,1,S),
  forth_to_asm(Rest,RestCode,S,NS),
  append([xchg(LocationA,LocationB)],RestCode,Code).
  
forth_to_asm([swap|Rest],
             Code,
             state(0,RegisterCount,RegisterShift,StackOffset,RegisterNames),
             NewState) :-
  !,
  get_first_register(Register,state(0,RegisterCount,RegisterShift,StackOffset,RegisterNames)),
  StackOffset1 is StackOffset - 1,
  State1 = state(1,RegisterCount,RegisterShift,StackOffset1,RegisterNames),
  forth_to_asm([swap|Rest],RestCode,State1,NewState),
  append([pop(Register)],RestCode,Code).

forth_to_asm([intrinsic(Name,[Type1]) | Rest],Code,State,NewState) :-
  !,
  intrinsic_instructions(intrinsic(Name,[Type1]),Target,OperationCode),
  force_to_register_and_get_first(Target,Buffering,State,State1),
  forth_to_asm(Rest,RestCode,State1,NewState),
  appendAll([Buffering,OperationCode,RestCode],Code).
  
  
  
forth_to_asm(
  [intrinsic(Name,[Type1,Type2])|Rest],
  Code,
  State,
  NewState) :-

  intrinsic(Name,[Type1,Type2],_ReturnType),
  !,
  intrinsic_instructions(
    intrinsic(Name,[Type1,Type2]),
    Source,
    Destination,
    OperationCode),
  force_to_register_and_get_first(Source,Buffering,State,state(UtilizedRegisters,RegisterCount,RegisterShift,StackOffset,RegisterNames)),
  pick(Destination,1,state(UtilizedRegisters,RegisterCount,RegisterShift,StackOffset,RegisterNames)),
  NewUtilization is UtilizedRegisters - 1,
  State1 =
    state(NewUtilization,RegisterCount,RegisterShift,StackOffset,RegisterNames),
  forth_to_asm(Rest,RestCode,State1,NewState),
  appendAll([Buffering,OperationCode,RestCode],Code).
  
%forth_to_asm([label(Name)|Rest],[label(Name)|RestCode],State,NewState) :-
  %forth_to_asm(Rest,RestCode,State,NewState).

forth_to_asm([num(int(N))|Rest],Result,State,NewState) :-
  !,
  allocate_register(Register,Buffer,State,State1),
  forth_to_asm(Rest,RestCode,State1,NewState),
  appendAll([Buffer,[mov('$'(N),Register)],RestCode],Result).

forth_to_asm([num(float(N))|Rest],Result,State,NewState) :-
  !,
  allocate_register(Register,Buffer,State,State1),
  forth_to_asm(Rest,RestCode,State1,NewState),
  appendAll([Buffer,[mov('$'(N),Register)],RestCode],Result).

forth_to_asm([nip,nip|Rest],Code,R,NewR) :-
  !,
  forth_to_asm([nip(2)|Rest],Code,R,NewR).
  
forth_to_asm([nip|Rest],Code,R,NewR) :-
  !,
  forth_to_asm([nip(1)|Rest],Code,R,NewR).

forth_to_asm([nip(N),nip|Rest],Code,R,NewR) :-
  NewN is N + 1,
  !,
  forth_to_asm([nip(NewN)|Rest],Code,R,NewR).


forth_to_asm([nip(N)|Rest],Code,R,NewR) :-
  !,
  nip(N,StartCode,R,R1),
  forth_to_asm(Rest,RestCode,R1,NewR),
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

forth_to_x86(ArgCount,Forth,Assembly) :-
  start_state(ArgCount,Start),
  forth_to_asm(Forth,PseudoAssembly,Start,_EndState), !,
  ifVerbose((write(PseudoAssembly), nl)),
  ifVerbose(format("~n    Assembly Optimizations:~n~n")),
  assembly_optimize(20,PseudoAssembly,OptimizedPseudoAssembly), !,
  ifVerbose(format("~n    Done Optimizing~n~n")),
  (psuedo_asm_to_x64(OptimizedPseudoAssembly,Assembly) ->
    ifVerbose(format("~n    Done Translating PseudoAssembly~n~n"))
    ;
    
    format("~n    Unable to Translate PseudoAssembly~n~n"),
    fail).
    

%forth_to_asm([nip(N)|Rest],Code,Rest,R,NewR) :-
  


  
