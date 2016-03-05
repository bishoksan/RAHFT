:- module(genfta1, [main/1], []).

% Generate an FTA from a program, and an error trace.

:- use_module(library(dynamic)).
:- use_module(library(write)).
:- use_module(library(read)).
:- use_module(library(lists)).

:- use_module(chclibs(builtins)).
:- use_module(chclibs(load_simple)).
:- use_module(chclibs(linearize)).

:- include(chclibs(common)).

:- dynamic transition/2.

recognised_option('-prg',  program(R),[R]).
recognised_option('-o',    outputFile(R),[R]).

main(ArgV) :-
	cleanup,
	get_options(ArgV,Options,_),
	setOptions(Options,File,OutS),
	load_file(File),
	makeFTA,	
	showFTA(OutS),
	close(OutS).
	
makeTraceFTAs([T|Ts],K0,K2,OutS) :-
	T=counterexample(Trace),
	!,
	term2type4(Trace,[(L->_)|FTA],K0,K1),
	writeFTA([(L->errortrace)|FTA],OutS),
	makeTraceFTAs(Ts,K1,K2,OutS).
makeTraceFTAs([_|Ts],K0,K1,OutS) :-
	makeTraceFTAs(Ts,K0,K1,OutS).
makeTraceFTAs([],K,K,_).
	
setOptions(Options,File,OutS) :-
	(member(program(File),Options); 
			write(user_output,'No input file given.'),
			nl(user_output), 
			fail),
	(member(outputFile(OutFile),Options), open(OutFile,write,OutS); 
			OutS=user_output).
			
cleanup :-
	retractall(my_clause(_,_,_)),
	retractall(transition(_,_)).
	
			
makeFTA :-
	my_clause(H,B,Id),
	functor(H,P,_),
	%predArity(P,N,PN),
	separate_constraints(B,_,Bs),
	getPreds(Bs,Qs,_),
	LHS =.. [Id|Qs],
	%AnyLHS =.. [Id|Any],
	assertz(transition(LHS,P)),
	%assertz(transition(AnyLHS,any)),
	fail.
makeFTA.

getPreds([],[],[]).
getPreds([B|Bs],[P|Qs],[any|As]) :-
	functor(B,P,_),
	%predArity(P,N,PN),
	getPreds(Bs,Qs,As).

showFTA(S) :-
	transition(Left,Right),
	write(S, (Left -> Right)),
	write(S,'.'),
	nl(S),
	fail.
showFTA(_).
	
separate_constraints([],[],[]).
separate_constraints([B|Bs],[C|Cs],Ds) :-
	constraint(B,C),
	!,
	separate_constraints(Bs,Cs,Ds).
separate_constraints([B|Bs],Cs,[B|Ds]) :-
	separate_constraints(Bs,Cs,Ds).

predArity(P,N,PN) :-
	name(P,X),
	name(N,Y),
	append(X,[95|Y],Z),
	name(PN,Z).
	


term2type(A,Def) :-
	term2type6(A,_,Def,[],0,_).
	
term2type4(A,Def,K0,K1) :-
		term2type6(A,_,Def,[],K0,K1).
	
termlist2type(Xs,T) :-
	argtypes(Xs,_,T,[],0,_).


term2type6(T, dynamic,As,As,K,K) :-
        var(T),
        !.
term2type6(T, R,[(L -> R)|As],As1,K,K2) :-
        newname(K,R),
        K1 is K+1,
        T =.. [F|Xs],
        argtypes(Xs,Qs,As,As1,K1,K2),
        L =.. [F|Qs].

argtypes([],[],As,As,K,K).
argtypes([T|Ts],[Q|Qs],As0,As2,K0,K2) :-
        term2type6(T,Q,As0,As1,K0,K1),
        argtypes(Ts,Qs,As1,As2,K1,K2).
        
newname(K,P) :-
	name(K,KN),
	append("errortrace",KN,QKN),
	name(P,QKN).

writeFTA([],_).
writeFTA([T|Ts],S) :-
	write(S,T),
	write(S,'.'),
	nl(S),
	writeFTA(Ts,S).

readTerms(S,Ts) :-
	read(S,C),
	(C==end_of_file -> Ts=[];
	 Ts=[C|Ts1],
	 readTerms(S,Ts1)).