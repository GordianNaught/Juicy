:- module(
  register_utils,
  [
    push_all_registers/3,
    force_to_register_and_get_first/4,
    get_register/3,
    get_first_register/2,
    allocate_register/4,
    nip/4,
    drop/4,
    remove_all_but/4,
    pick/3,
    pick_from_top/3,
    move_n_to_top/4
  ]).

:- use_module(utils).

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

% at least on utilized register
% less nips than utilized registers
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

% at least one utilized register
% nip count more than or equal to utilized registers
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

% no utilized registers
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
