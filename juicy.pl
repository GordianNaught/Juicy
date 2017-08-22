:- module(juicy, [compile/1,compile/2,repl/0,start/1,infer_file/1]).

:- use_module(juicy_tokenize, [tokenize/2]).
:- use_module(juicy_parse, [parse/2]).
:- use_module(juicy_compile, [compile_program/2]).
:- use_module(juicy_forth_to_string, [forth_to_string/2]).
:- use_module(juicy_inference, [infer_program/2]).
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
  infer_program(Ast,Definitions),
  write(inferrences=Definitions), nl,
  compile_program(Definitions,Forths),
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

infer_file(FileName) :-
  read_file_to_string(FileName,String,[]),
  tokenize(String,Tokens),
  parse(Tokens,Ast),
  write(infer_program(Ast, Inferrences)), nl,
  infer_program(Ast, Inferrences),
  write(Inferrences).
  
write_to_file(string(S), OutputFile) :-
  open(OutputFile,write,Stream),
  write(Stream,S),
  close(Stream).
  
compile_file(InputFile, OutputFile) :-
  write(compile_file(InputFile,OutputFile)),
  compile_file_asm_string(InputFile,AsmCode),
  write_to_file(string(AsmCode), OutputFile).
  
assemble(Assembler,SourceFile,ExecutableName) :-
  !,
  format(string(Command),
         "~w ~w -o ~w",
         [Assembler,SourceFile,ExecutableName]),
  shell(Command,_ReturnValue),
  !.

inputFile_check(Arguments) :-
  ground(Arguments.inputFile)
  ;
  write("ERROR: An input file must be supplied.\n"),
  write(Arguments), nl,
  fail.
  
check_arguments(Arguments) :-
  inputFile_check(Arguments).

% TODO: remove dictionary usage for GNU Prolog compatibility
start([_ProgramName|Args]) :-
  write('parsing arguments'), nl,
  parse_args(Args,Dict),
  Dict = args{outputFile:OutputFile,
              inputFile:InputFile,
              assembler:Assembler,
              verbose:_Verbose},
  check_arguments(Dict),
  compile_file(InputFile,OutputFile),
  ExecutablePath = 'Compile/executable',
  assemble(Assembler,OutputFile,ExecutablePath),
  format("~w created\n", [ExecutablePath]).
  
parse_argument('-o',outputFile).
parse_argument('-i',inputFile).
parse_argument('-a',assembler).
parse_argument('-v',verbose).

parse_args(Args,ArgDict) :-
  DefaultDict = args{verbose:false,
                     outputFile:'output.s',
                     inputFile:_,
                     assembler:'gcc'},
  parse_args(Args,DefaultDict,ArgDict).

parse_args([],Arguments,Arguments).
parse_args([Flag,Argument|Rest],StartDict,FinalDict) :-
  parse_argument(Flag,FlagName),
  NewDict = StartDict.put([FlagName=Argument]),
  parse_args(Rest,NewDict,FinalDict).


% TODO: allow args like -abc
