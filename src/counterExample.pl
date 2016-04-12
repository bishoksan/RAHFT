:- module(counterExample, [checkCounterExample/3, main/1], []).

:- use_module(library(read)).
:- use_module(library(write)).
:- use_module(library(lists)).

:- use_module(chclibs(linearize)).
:- use_module(chclibs(ppl_ops)).
:- use_module(chclibs(input_ppl_clausenum)).
:- use_module(chclibs(common)).

main([F, TraceF,Result]) :-
	unsafe(F,TraceF, Result).
	
unsafe(F,PFile, Result) :-
	open(PFile,read,S),
	read(S,C),
	existsCex(S,C,Cex),
	close(S),
	checkCounterExample(Cex,F, Result).
	
existsCex(_,end_of_file,no) :-
	!.
existsCex(_,(cex(Cex)),Cex) :-
	!,
	write(user_output,Cex),
	nl(user_output).
existsCex(_,(counterexample(Cex)),Cex) :-
	!,
	write(user_output,Cex),
	nl(user_output).
existsCex(S,_,Cex) :-
	read(S,C1),
	existsCex(S,C1,Cex).
	
checkCounterExample(no,_, Result) :-
	!,
	Result=safe.
checkCounterExample(Cex,F, Result) :-
	start_ppl,
	load_file(F),
	( checkTrace([false],[],[Cex])
	; checkTrace([false_ans],[],[Cex])
	),
	!,
	end_ppl,
	Result=unsafe.
checkCounterExample(_,_, Result) :-
	end_ppl,
	Result=unknown.
	
checkTrace([],_,_).
checkTrace([B|Bs],Cs,[T|Ts]) :-
	T =..[C|Ts1],
	my_clause(B,Bs1,C),
	separate_constraints(Bs1,Cs1,Bs2),
	append(Bs2,Bs,Bs3),
	append(Cs1,Cs,Cs2),
	checkSat(Cs2),
	append(Ts1,Ts,Ts2),
	checkTrace(Bs3,Cs2,Ts2).
	
checkSat(Cs) :-
	\+ nonSat(Cs).

nonSat(Cs) :-
	numbervars(Cs,0,_),
	\+ satisfiable(Cs,_).

	
