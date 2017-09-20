% This module contains the interface to the
% main functionality of the Juicy Compiler.
:- module(
  juicy,
  [
    compile/2,
    start/1,
    infer_file/1
  ]).

:- use_module(juicy_tokenize, [tokenize/2]).
:- use_module(juicy_parse, [parse/2]).
:- use_module(juicy_compile, [compile_program/2]).
:- use_module(juicy_forth_to_string, [forth_to_string/2]).
:- use_module(juicy_inference, [infer_program/2]).
:- use_module(utils).
:- use_module(juicy_global).

% This is the name of the label of the main
% function in the generated assembly.
juicyMainName(Name) :-
  cleanLabel((main,[]),Name).

% This the the generated main function in the
% assembly that uses C calling conventions but
% calls the Juicy main function in a way it
% understands.
mainAsm(Func) :-
  juicyMainName(JuicyMain),
  format(
    string(Func),
"main:
  pushq $returnLoc
  jmp ~w
returnLoc:
  movl %eax, %edi
  call exit
",
    [JuicyMain]).

% Convert symbolic pseudo-assembly into real
% x64 assembly.
symbolic_asm_to_asm(function(Name,ArgTypes,Instructions),Program) :-
  symbolic_instructions_to_asm(Instructions,Asm),
  !,
  cleanLabel((Name,ArgTypes),Label),
  format(string(Program),"~w:\n~w",[Label,Asm]).

% Convert a list of symbolic pseudo-assembly
% instructions into real x64 assembly.
symbolic_instructions_to_asm([],"") :- !.
symbolic_instructions_to_asm([I|Rest],Asm) :-
  symbolic_instructions_to_asm(Rest,RestAsm),
  !,
  format(string(Asm),"  ~w\n~w", [I,RestAsm]).

% Convert a list of functions compiled to
% pseudo-assembly into x64 assembly.
symbolic_program_to_asm(Parts,Result) :-
  Goal =
    (
      symbolic_asm_to_asm(Part,Asm),
      format(string(String),"~w",[Asm])
    ),
  do_each(Parts,Part,Goal,String,Strings),
  append_strings_delimited(Strings,Result,"").

forth_to_asm(Forth,Asm) :-
  forth_to_string(Forth,Code),
  ifVerbose((write(Code), nl)),
  symbolic_program_to_asm(Forth,Asm).

% Take a program as a string and return the
% compiled assembly as a string.
compile(String,AsmString) :-
  tokenize(String,Tokens),
  parse(Tokens,Ast),
  infer_program(Ast,Definitions),
  ifVerbose(format("BEGIN COMPILE\n")),
  compile_program(Definitions,Forths),
  do_each(Forths,Forth,forth_to_asm(Forth,Asm),Asm,Asms),
  append_strings_delimited(Asms,Code,"\n"),
  mainAsm(MainAsm),
  format(string(DataSection), ".section .data\n", []),
  format(string(EpInit), "ep_init:~n", []),
  format(string(BlockBuffers), "block_buffers:\n", []),
  format(string(SectionBSS), ".section .bss\n", []),
  format(string(SectionText), ".section .text\n", []),
  format(string(ProgramCode), ".globl main\n\n~w\n~w",
                              [Code,MainAsm]),
  format(
    string(AsmString),
    "~w~w~w~w~w~w",
    [
      DataSection,
      EpInit,
      BlockBuffers,
      SectionBSS,
      SectionText,
      ProgramCode
    ]),
  ifVerbose((nl,nl,format(AsmString),nl,nl)).

compile_file_asm_string(FileName,Code) :-
  read_file_to_string(FileName,String,[]),
  compile(String,Code).

infer_file(FileName) :-
  read_file_to_string(FileName,String,[]),
  tokenize(String,Tokens),
  parse(Tokens,Ast),
  infer_program(Ast, _Inferrences).

% Write a string to a file.
write_to_file(string(S), OutputFile) :-
  open(OutputFile,write,Stream),
  write(Stream,S),
  close(Stream).

% Take one file's contents, compile them,
% then emit the assembly into another file.
compile_file(InputFile, OutputFile) :-
  compile_file_asm_string(InputFile,AsmCode),
  write_to_file(string(AsmCode), OutputFile).

% Call the assembler on a file containing the
% assembly to generate an executable in another file.
% TODO: get rid of assembly intermediate file?
assemble(Assembler,SourceFile,ExecutableName) :-
  !,
  format(string(Command),
         "~w ~w -o ~w",
         [Assembler,SourceFile,ExecutableName]),
  shell(Command,_ReturnValue),
  !.

% Make sure an input file is specified in the
% arguments to the compiler.
inputFile_check(Arguments) :-
  (ground(Arguments.inputFile) ->
    true
    ;
    write("ERROR: An input file must be supplied.\n"),
    fail).

% Check the arguments given to the compiler.
check_arguments(Arguments) :-
  inputFile_check(Arguments).

% TODO: remove dictionary usage for GNU Prolog compatibility
start([ProgramName|Args]) :-
  parse_args(Args,Dict),
  !,
  (Dict.time ->
    time(start_internal(ProgramName, Dict))
    ;
    start_internal(ProgramName, Dict)).

% This is the main predicate of the Juicy Compiler.
% It has the `_internal' suffix to distinguish it
% from the entry point of the compiler's executable.
start_internal(_ProgramName, Dict) :-
  Dict = args{outputFile:OutputFile,
              inputFile:InputFile,
              assembler:Assembler,
              time:_Time,
              verbose:Verbose},
  check_arguments(Dict),
  (Verbose ->
    assert_verbose
    ;
    true),
  ifVerbose((write(Dict), nl)),
  format(string(AssemblyName), "~w.s", [OutputFile]),
  compile_file(InputFile,AssemblyName),
  ExecutablePath = OutputFile,
  assemble(Assembler,AssemblyName,ExecutablePath),
  format("~w created\n", [ExecutablePath]).

% This is a fall-through predicate to indicate
% there was a failure to compile.
start_internal(_ProgramName, _Dict) :-
  nl, format("FAILURE TO COMPILE~n").

% These declare the arguments the compiler can
% accept, the internal symbols/atoms they are
% associated with for use in the compiler, and
% whether they are flags or specifiers.
%
% Specifiers expect an argument after them,
% flags do not.
parse_argument('-o',outputFile,specifier).
parse_argument('--output-file',outputFile,specifier).
parse_argument('-i',inputFile,specifier).
parse_argument('--input-file',inputFile,specifier).
parse_argument('-a',assembler,specifier).
parse_argument('--assembler',assembler,specifier).
parse_argument('-v',verbose,flag).
parse_argument('--verbose',verbose,flag).
parse_argument('-q',quiet,flag).
parse_argument('--quiet',quiet,flag).
parse_argument('-t',time,flag).
parse_argument('--time',time,flag).

parse_args(Args,ArgDict) :-
  DefaultDict = args{verbose:false,
                     time:false,
                     outputFile:'a.out',
                     inputFile:_,
                     assembler:'gcc'},
  parse_args(Args,DefaultDict,ArgDict).

parse_args([],Arguments,Arguments) :- !.
parse_args([Specifier,Argument|Rest],StartDict,FinalDict) :-
  parse_argument(Specifier,SpecifierName,specifier),
  !,
  NewDict = StartDict.put([SpecifierName=Argument]),
  parse_args(Rest,NewDict,FinalDict).
parse_args([Flag|Rest],StartDict,FinalDict) :-
  parse_argument(Flag,FlagName,flag),
  !,
  NewDict = StartDict.put([FlagName=true]),
  parse_args(Rest,NewDict,FinalDict).
parse_args([InputFile|Rest],StartDict,FinalDict) :-
  not(ground(StartDict.inputFile)),
  NewDict = StartDict.put([inputFile=InputFile]),
  parse_args(Rest,NewDict,FinalDict).
parse_args(Given,_,_) :-
  format("unable to parse arguments, parsing failed at ~w",
         [Given]),
  !,
  fail.


% TODO: allow args like -abc
