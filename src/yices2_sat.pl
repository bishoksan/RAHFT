:- module(yices2_sat, [
	yices_sat/2,
	yices_unsat/2,
	yices_model/3,
	true_in_model/2,
	get_value_as_term/3], []).

:- use_module(library(write)).
:- use_module(library(lists)).
:- use_module(library(strings)).

:- use_module(ciao_yices(ciao_yices_2)).

yices_sat(E,Vars) :-
	expr2yices(E,Y),
	!,
	write_string(Y),nl,
	declareVars(Vars),
	yices_context(Ctx),
	yices_parse_term(Y,T),
	reportError(T),
	yices_assert_formula(Ctx,T,_Status),
	yices_check(Ctx,StatusName),
	%write(StatusName),nl,
	StatusName==satisfiable,
	yices_free_context(Ctx).
	
yices_unsat(E,Vars) :-
	expr2yices(E,Y),
	!,
	%write('SMT formula: '), nl,
    %write_string(Y),nl,
	declareVars(Vars),
	yices_context(Ctx),
	yices_parse_term(Y,T),
	yices_assert_formula(Ctx,T,_Status),
	yices_check(Ctx,StatusName),
	%write(StatusName),nl,
	StatusName==unsatisfiable,
	yices_free_context(Ctx).
	
yices_model(E,Vars,M) :-
	expr2yices(E,Y),
    %write('clp formula '), nl,
    %write(E), nl,
	!,
    %write('SMT formula: '), nl,
    %write_string(Y),nl,
	declareVars(Vars),
	yices_context(Ctx),
	yices_parse_term(Y,T),
	yices_assert_formula(Ctx,T,_Status),
	yices_check(Ctx,StatusName),
	StatusName==satisfiable,
    %flag to indicates whether the model should include eliminated variables
	yices_get_model(Ctx,1,M).

	
true_in_model(E,M) :-
	expr2yices(E,Y),
	!,
	yices_parse_term(Y,T),
	yices_formula_true_in_model(M,T,Status),
	Status==1.

get_value_as_term(M, E, Term) :-
	expr2yices(E,Y),
	!,
	yices_parse_term(Y,T),
	yices_get_value_as_term(M,T,Term).

	
expr2yices([X1,X2|Y],S) :-
	expr2yices(X1,S1),
	expr2yices([X2|Y],S2),
	concatStrings(["(","and ",S1," ",S2,")"],S).
expr2yices([false],"false").
expr2yices([X],S) :-
	expr2yices(X,S).
expr2yices([],"true").
expr2yices((X;Y),S) :-
	expr2yices(X,S1),
	expr2yices(Y,S2),
	concatStrings(["(","or ",S1," ",S2,")"],S).
expr2yices((X,Y),S) :-
	expr2yices(X,S1),
	expr2yices(Y,S2),
	concatStrings(["(","and ",S1," ",S2,")"],S).
expr2yices((X<Y),S) :-
	expr2yices(X,S1),
	expr2yices(Y,S2),
	concatStrings(["(","< ",S1," ",S2,")"],S).
expr2yices((X>Y),S) :-
	expr2yices(X,S1),
	expr2yices(Y,S2),
	concatStrings(["(","> ",S1," ",S2,")"],S).
expr2yices((X=<Y),S) :-
	expr2yices(X,S1),
	expr2yices(Y,S2),
	concatStrings(["(","<= ",S1," ",S2,")"],S).
expr2yices((X>=Y),S) :-
	expr2yices(X,S1),
	expr2yices(Y,S2),
	concatStrings(["(",">= ",S1," ",S2,")"],S).
expr2yices((X*Y),S) :-
	expr2yices(X,S1),
	expr2yices(Y,S2),
	concatStrings(["(","* ",S1," ",S2,")"],S).
expr2yices((X+Y),S) :-
	expr2yices(X,S1),
	expr2yices(Y,S2),
	concatStrings(["(","+ ",S1," ",S2,")"],S).
expr2yices((X-Y),S) :-
	expr2yices(X,S1),
	expr2yices(Y,S2),
	concatStrings(["(","- ",S1," ",S2,")"],S).
expr2yices(-(X),S) :-
	expr2yices(X,S1),
	concatStrings(["(","- ",S1,")"],S).
expr2yices((X=Y),S) :-
	expr2yices(X,S1),
	expr2yices(Y,S2),
	concatStrings(["(","= ",S1," ",S2,")"],S).
expr2yices(xor(X,Y),S) :-
	expr2yices(X,S1),
	expr2yices(Y,S2),
	concatStrings(["(","xor ",S1," ",S2,")"],S).
expr2yices((X->Y),S) :-
	expr2yices(X,S1),
	expr2yices(Y,S2),
	concatStrings(["(","=> ",S1," ",S2,")"],S).
expr2yices(neg(X),S) :-
	expr2yices(X,S1),
	concatStrings(["(","not ",S1,")"],S).
expr2yices('$VAR'(N),S) :-
	name(N,I),
	append("x",I,S).
expr2yices(A,S) :-
	atomic(A),
	name(A,S).
expr2yices(A,"") :-
	write('problem with '),
	write(A),
	nl.
	

declareVars([(X,real)|Vars]) :-
	expr2yices(X,V),
	!,
	yices_declare_real(V),
	declareVars(Vars).
declareVars([(X,int)|Vars]) :-
	expr2yices(X,V),
	!,
	yices_declare_int(V),
	declareVars(Vars).
declareVars([(X,bool)|Vars]) :-
	expr2yices(X,V),
	!,
	yices_declare_bool(V),
	declareVars(Vars).
declareVars([]).


concatStrings([],"").
concatStrings([S|Ss],R) :-
	concatStrings(Ss,R1),
	append(S,R1,R).
	
reportError(S) :-
	S<0 -> reportErrorState; true.
	
reportErrorState :-
	yices_error_string(E),
	write_string(E),
	nl.
