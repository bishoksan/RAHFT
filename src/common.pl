
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



separate_constraints([],[],[]).
separate_constraints([B|Bs],[C|Cs],Ds) :-
	constraint(B,C),
	!,
	separate_constraints(Bs,Cs,Ds).
separate_constraints([B|Bs],Cs,[B|Ds]) :-
	separate_constraints(Bs,Cs,Ds).

constraint(X=Y, X=Y).
constraint(X=:=Y, X=Y).
constraint(X is Y, X = Y).
constraint(X>Y, X>Y).
constraint(X>=Y, X>=Y).
constraint(X=<Y, X=<Y).
constraint(X<Y, X<Y).

constraint(_\==_,0=0).
constraint(_=\=_,0=0).
constraint(true,0=0).
constraint(fail,1=0).


list2Conj([A], (A)):-
    !.
list2Conj([A|R], (A,R1)):-
    !,
list2Conj(R, R1).
list2Conj([], (true)). % meaning true

listofList2Disj([A], (A1)):-
    !,
    list2Conj(A, A1).
listofList2Disj([A|R], ((A1);R1)):-
    !,
    list2Conj(A, A1),
    listofList2Disj(R, R1).
listofList2Disj([], (1=0)). %meaning false


number_atom(N, A) :- number_codes(N, C), atom_codes(A, C).

max_member([X], X).
max_member([X|R], M):-
    !,
    max_member(R, Max),
    max(X,Max, M).

max(X, Y, X):-
    X>=Y,
    !.
max(_, Y, Y).

convert2num(A,A) :-
	number(A),
	!.
convert2num(A,A1) :-
	atom(A),
	atom_number(A,A1).

dummyCList([],[]).
dummyCList([C|Cs],[C=C|Cs1]) :-
    dummyCList(Cs,Cs1).

%this is for removing the brackets
constraint1(X, Xs):-
    functor(X, P, _),
    name(P, [44]),
    conj2List(X, Xs). %44 is for commas


%this is to avoid recognizing "," as a functor rather than a collection of constraints
conj2List((A, (B)), [A|R]):-
    !,
    conj2List(B,R).
conj2List((A), [A]).
