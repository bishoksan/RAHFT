:- module(cextrace,_).

:- use_module(linearize).
:- use_module(ppl_ops).
:- use_module(input_ppl_clausenum).


main([F,PFile]) :-
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
existsCex(S,_,Cex) :-
	read(S,C1),
	existsCex(S,C1,Cex).
	
checkCounterExample(no,_) :-
	!,
	fail.
checkCounterExample(Cex,F) :-
	start_ppl,
	load_file(F),
	(checkTrace([false],[],[Cex]);
	 checkTrace([false_ans],[],[Cex])
	),
	!,
	end_ppl.
checkCounterExample(_,_) :-
	end_ppl,
	fail.
	
checkTrace([],_,_).
checkTrace([B|Bs],Cs,[T|Ts]) :-
	T =..[C|Ts1],
	my_clause(B,Bs1,C),
	separate_constraints(Bs1,Cs1,Bs2),
	checkTrace([B],Cs1,[T]),
	checkTrace(Bs,Cs2,Ts).
	
separate_constraints([],[],[]).
separate_constraints([B|Bs],[C|Cs],Ds) :-
	constraint(B,C),
	!,
	separate_constraints(Bs,Cs,Ds).
separate_constraints([B|Bs],Cs,[B|Ds]) :-
	separate_constraints(Bs,Cs,Ds).
	
