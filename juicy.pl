:- module(juicy, [compile/1,compile/2,repl/0,main/1]).

:- use_module(juicy_tokenize, [tokenize/2]).
:- use_module(juicy_parse, [parse/2]).
:- use_module(juicy_compile, [compile_program/2]).
:- use_module(juicy_forth_to_string, [forth_to_string/2]).
:- use_module(utils).

symbolic_asm_to_asm(function(Name,Instructions),Program) :-
  symbolic_instructions_to_asm(Instructions,Asm),
  !,
  format(string(Program),"~w:\n~w",[Name,Asm]).

symbolic_instructions_to_asm([],"") :- !.
symbolic_instructions_to_asm([I|Rest],Asm) :-
  symbolic_instructions_to_asm(Rest,RestAsm),
  !,
  format(string(Asm),"  ~w\n~w", [I,RestAsm]).

symbolic_program_to_asm(Parts,Result) :-
  Goal = (juicy:symbolic_asm_to_asm(Part,Asm),format(string(String),"~w",[Asm])),
  do_each(Parts,Part,Goal,String,Strings),
  append_strings_delimited(Strings,Result,"").

forth_to_asm(Forth,Asm) :-
  forth_to_string(Forth,Code),
  write(Code), nl,
  symbolic_program_to_asm(Forth,Asm), nl.
  
compile(String,Proper) :-
  tokenize(String,Tokens),
  write(tokens=Tokens), nl,
  parse(Tokens,Ast),
  write(ast=Ast), nl,
  compile_program(Ast,Forths),
  do_each(Forths,Forth,juicy:forth_to_asm(Forth,Asm),Asm,Asms),
  append_strings_delimited(Asms,Code,"\n"),
  format(
    string(Proper),
    ".section .data\nep_init:~nblock_buffers:\n.section .bss\n.section .text\n.globl main\n~w\n",
    [Code]),
  format(Proper),nl,nl.
  
repl :-
  read_line_to_string(user_input,String),
  compile(String,_Code),
  repl.

compile(FileName) :-
  read_file_to_string(FileName,String,[]),
  compile(String,_Code).
  
compile_file_asm_string(FileName,Code) :-
  read_file_to_string(FileName,String,[]),
  compile(String,Code).
  
write_to_file(string(S), OutputFile) :-
  open(OutputFile,write,Stream),
  write(Stream,S),
  close(Stream).
  
compile_files(InputFile, OutputFile) :-
  write(compile_files(InputFile,OutputFile)),
  compile_file_asm_string(InputFile,AsmCode),
  write_to_file(string(AsmCode), OutputFile).
  
assemble(Assembler,SourceFile,ExecutableName) :-
  !,
  format(string(Command),
         "~w ~w -o ~w",
         [Assembler,SourceFile,ExecutableName]),
  shell(Command,_ReturnValue),
  !.

main([_ProgramName|Args]) :-
  parse_args(Args,Dict),
  Dict = args{outputFile:OutputFile,
              inputFile:InputFile,
              assembler:Assembler,
              verbose:_Verbose},
  compile_files(InputFile,OutputFile),
  assemble(Assembler,OutputFile,'Compile/executable').
  
parse_argument('-o',outputFile).
parse_argument('-i',inputFile).
parse_argument('-a',assembler).
parse_argument('-v',verbose).

parse_args(Args,ArgDict) :-
  DefaultDict = args{verbose:false,
                     outputFile:'output.s',
                     assembler:'gcc'},
  parse_args(Args,DefaultDict,ArgDict).

parse_args([],Dict,Dict).
parse_args([Flag,Argument|Rest],StartDict,FinalDict) :-
  parse_argument(Flag,FlagName),
  NewDict = StartDict.put([FlagName=Argument]),
  parse_args(Rest,NewDict,FinalDict).


% TODO: allow args like -abc
