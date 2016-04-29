:- module(stripSuffix, [main/1], []).

:- use_module(library(dynamic)).
:- use_module(library(write)).
:- use_module(library(read)).
:- use_module(library(lists)).

:- use_module(chclibs(timer_ciao)).
:- use_module(chclibs(common)).

:- include(chclibs(get_options)).

:- data flag/1. % TODO: use
:- dynamic(prop/2).
:- dynamic(atomicprops/0).

recognised_option('-o',    outputFile(R),[R]).
recognised_option('-props',propFile(R),[R]).
recognised_option('-atoms',atomicprops,[]).
	
main(ArgV) :-
	cleanup,
	setOptions(ArgV,OutS),
	start_time,
	showallprops_qa_strip(OutS),
	end_time(user_output),
	nl(OutS),
	close(OutS).	
	
setOptions(ArgV,OutS) :-
	get_options(ArgV,Options,_),
	(member(outputFile(OutFile),Options), open(OutFile,write,OutS); 
			OutS=user_output),
	(member(propFile(PFile),Options), readPropFile(PFile); 
			true),
	(member(atomicprops,Options), assertz(atomicprops); 
			true).

cleanup :-
	retractall(prop(_,_)),
	retractall(atomicprops).
	
readPropFile(PFile) :-
	open(PFile,read,S),
	read(S,C),
	readPropFacts(S,C),
	close(S).
	
readPropFacts(_,end_of_file) :-
	!.
readPropFacts(S,(H:-C)) :-
	assertz(prop(H,C)),
	read(S,C1),
	readPropFacts(S,C1).

showallprops_qa_strip(S) :-
	prop(F,C),
	numbervars((F,C),0,_),
	stripSuffix(F,F1),
	(atomicprops -> write_each_atom_prop(C,F1,S); 
	    writeClause(F1,C,S)),
	fail.
showallprops_qa_strip(_).

writeClause(F1,C,S) :-
	writeq(S,F1), 
	write(S,' :- '), 
	write(S,C),
	write(S,'.'),
	nl(S),
	!.

stripSuffix(F,F1) :-
	F =.. [P|Xs],
	name(P,PName),
	removeSuffixChars(PName,P1Name),
	name(P1,P1Name),
	F1 =.. [P1|Xs],
	!.
	
removeSuffixChars(FName,F1Name) :-
	append("_query",_,Suff),
	append(F1Name,Suff,FName),
	!.
removeSuffixChars(FName,F1Name) :-
	append("_ans",_,Suff),
	append(F1Name,Suff,FName),
	!.
removeSuffixChars(FName,FName).

	
write_each_atom_prop([],B,S) :-
	!,
	writeClause(B,[],S).
write_each_atom_prop([C],B,S) :-
	!,
	writeClause(B,[C],S).
write_each_atom_prop([C|Cs],B,S) :-
	writeClause(B,[C],S),
	write_each_atom_prop(Cs,B,S).

