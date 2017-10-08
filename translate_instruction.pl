:- module(translate_instruction, [translate_instruction/2]).

translate_location(indirect(Thing),String) :-
  !,
  translate_location(Thing,ThingStr),
  format(string(String),"*~w",[ThingStr]).

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
  format(string(String),
         "~w   ~w, ~w",
         [InstructionNameString, Location1String, Location2String]).

translate_instruction(Instruction,String) :-
  Instruction =.. [InstructionName, Location],
  !,
  translate_instruction_name(InstructionName, InstructionNameString),
  !,
  translate_location(Location, LocationString),
  format(string(String),"~w   ~w",[InstructionNameString, LocationString]).
  
translate_instruction(NotFound,_) :-
  format("cannot translate assembly instruction: ~w~n",[NotFound]), !, fail.
