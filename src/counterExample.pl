:- module(counterExample,_).

:- use_module(linearize).
:- use_module(ppl_ops).
:- use_module(input_ppl_clausenum).

main([F]) :-
	unsafe(F,'traceterm.out').
	
unsafe(F,PFile) :-
	open(PFile,read,S),
	read(S,C),
	existsCex(S,C,Cex),
	close(S),
	checkCounterExample(Cex,F).
	
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
	
checkCounterExample(no,_) :-
	!,
	write('No counterexamples: program is safe'),
	nl,
	halt(0).
checkCounterExample(Cex,F) :-
	start_ppl,
	load_file(F),
	(checkTrace([false],[],[Cex]);
	 checkTrace([false_ans],[],[Cex])
	),
	!,
	end_ppl,
	write('Program is unsafe'),
	nl,
	halt(0).
checkCounterExample(_,_) :-
	end_ppl,
	write('Spurious counterexample'),
	nl,
	halt(1).
	
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
	
separate_constraints([],[],[]).
separate_constraints([B|Bs],[C|Cs],Ds) :-
	constraint(B,C),
	!,
	separate_constraints(Bs,Cs,Ds).
separate_constraints([B|Bs],Cs,[B|Ds]) :-
	separate_constraints(Bs,Cs,Ds).
	
