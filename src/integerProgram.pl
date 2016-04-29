:- module(integerProgram,[main/1],[]).

:- use_module(library(lists)).
:- use_module(chclibs(common), [separate_array_constraints/3]).
:- use_module(library(sort)).
:- use_module(library(write)).
:- use_module(library(aggregates)).
:- use_module(library(dynamic)).
:- use_module(chclibs(load_simple)).

:- include(chclibs(get_options)).

:- data flag/1. % TODO: use
:- dynamic new_clause/2.

% Remove read/3 and write/4 constraints (for the array theory) from
% the program.

% Command line usage:
%   integerProgram -prg in.pl -o out.pl

main(ArgV) :-
	cleanup,
	setOptions(ArgV,File,OutS),
	load_file(File),
	operator,
	writeClauses(OutS),
	close(OutS).

setOptions(ArgV,File,OutS) :-
	get_options(ArgV,Options,_),
	( member(programO(File),Options) -> true
	; write(user_output,'No input file given.'),nl(user_output),fail
	),
	( member(outputFile(OutFile),Options) -> open(OutFile,write,OutS)
	; OutS=user_output
	).

recognised_option('-prg',  programO(R),[R]).
recognised_option('-o',    outputFile(R),[R]).

cleanup :-
	retractall(my_clause(_,_,_)),
	retractall(new_clause(_,_)).

% ---------------------------------------------------------------------------

operator :-
	my_clause(Head,B,_),
	separate_array_constraints(B, _, B2),
%	display(my_clause(Head,B2)), nl,
	record(Head,B2),
	fail.
operator.

record(Head,H):-
	assertz(new_clause(Head,H)).

% ---------------------------------------------------------------------------

writeClauses(S) :-
	new_clause(A,Body),
	numbervars((A,Body),0,_),
	writeq(S,A),
	write(S,' :- '),
	nl(S),
	writeBodyClauses(Body,S),
	write(S,'.'),
	nl(S),
	fail.
writeClauses(_).
	
writeBodyClauses([],S) :-
	write(S,'      '),
	write(S,true).
writeBodyClauses([A],S) :-
	!,
	write(S,'      '),
	writeq(S,A).
writeBodyClauses([A|As],S) :-
	write(S,'      '),
	writeq(S,A),
	write(S,','),
	nl(S),
	writeBodyClauses(As,S).

