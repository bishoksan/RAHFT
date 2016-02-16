% get_options/3 provided by Michael Leuschel
get_options([],[],[]).
get_options([X|T],Options,Args) :-
	( recognised_option(X,Opt,Values) ->
	    append(Values, Rest, T),
	    RT = Rest,
	    Options = [Opt|OT], Args = AT
	; Options = OT, Args = [X|AT],
	  RT = T
	),
	get_options(RT,OT,AT).


list2Conj([A], (A)):-
    !.
list2Conj([A|R], (A,R1)):-
    !,
    list2Conj(R, R1).
list2Conj([], (1=1)). % meaning true

list2Disj([A], (A1)):-
    !,
    list2Conj(A, A1).
list2Disj([A|R], ((A1);R1)):-
    !,
    list2Conj(A, A1),
    list2Disj(R, R1).
list2Disj([], (1=0)).

convert2num(A,A) :-
	number(A),
	!.
convert2num(A,A1) :-
	atom(A),
	atom_number(A,A1).




