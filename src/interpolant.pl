% 
% The predicate interpolant/4 computes an interpolant of two sets of
% contraints which are unsat together.
% 
% The implementation is based on the algorithm presented in the paper:
% Constraint Solving for Interpolation by Andrey Rybalchenko and Viorica
% Sofronie-Stokkermans (VMCAI'07)
% 
% Implementation: for 2nd and 3rd branch, we take One of LambdaLt >0 or
% One of MuLt >0
% 
% This example gives is not captured by any one of the three branch in
% the algorithm if we do not use equality constraints over integers.
% 
% computeInterpolant([B,D],[D>100,B=D-10],[D>90, D=<101,B>91],Ints),
%

:- module(interpolant, [makeRealVars/2, computeInterpolant/4], []).

:- use_module(library(write)).

:- use_module(library(ppl)).
:- use_module(library(terms_vars)).
:- use_module(library(strings)).
:- use_module(library(read_from_string)).   % converting to string to rational, real or any number

:- use_module(canonical).
:- use_module(lcm).
:- use_module(setops).
:- use_module(normalize_constraints).
:- use_module(library(lists)).
:- use_module(ppl_ops).
:- use_module(linearize).
:- use_module(yices2_sat).
%:- use_module(ciao_yices_2).
:-use_module(ciao_yices(ciao_yices_2)).


testall :-

	numbervars([X,Y,Z],0,_),
	go([X,Y,Z],[Z<0, X =< Z, Y =< X],[Y=<0, X+Y>=0])
,

	go([X,Y,Z],[Y=<0, X+Y>=0],[Z<0, X =< Z, Y =< X]),
	go([X,Y], [X=0, Y=0], [X > Y]),
	go([X,Y], [X=0, Y=0], [X=0, Y=0, X > Y]),
	go([X,Y], [X > Y], [X=0, Y=0]),
	go([X], [X >= 0], [X < 0]),
	go([X], [X < 0], [X >= 0]),

	go([X], [X < -2], [X >= 0])
,
	go([X], [X > -1, X =< 1], [X > 1]),
	go([X,Y,Z], [X = 0, Y=Z], [X+Y > Z]),
	go([X,Y],[X=Y], [X > Y]),
	numbervars([A,B,C,D,E,F,G,H,I,J],0,_),
	go([A,B,C,D,E],[D>=0,C=0,D=1,B=D],[A>B,A=E+1,E=0,C=0]),
	go([A,B,C,D,E],[D>=0,C=0,D=1,B=D],[A<B,A=E+1,E=0,C=0]),
	go([A,B,C,D,E],[C=0,D=B,D>=0,E=0,C=0,A=E+1,D=1],[A>B,C=C,D=D,E=E]),
	go([A,B,C,D,E,F,G],[C=0,D=E,D>=0,F=C+1,B=E+1,G=0,F=0,A=G+1,D=1],[A>B, C=C, D=D, E=E,F=F, G=G]),
	go([A,B,C,D,E,F,G],[A>B, C=C, D=D, E=E,F=F, G=G],[C=0,D=E,D>=0,F=C+1,B=E+1,G=0,F=0,A=G+1,D=1]),
	go([A,B,C,D,E,F,G,H,I,J],[C=0,D=0,E=0,F=0,G=0,H=1+I,A=H+E,J=1+F,C=I+G,B=J+D],[A<B]),
	go([A,B,C,D,E,F,G,H,I,J],[C=0,D=0,E=0,F=0,G=0],[A<B,H=1+I,A=H+E,J=1+F,C=I+G,B=J+D]),
	go([A,B,C,D,E],[E=0,D=B,D>=0], [E=0,D=B,D>=0,C=0,E=0,A=C+1,D=1,A>B]),
	go([A,B,C,D,E],[E=0,D=B,D>=0], [C=0,E=0,A=C+1,D=1,A>B]),
	go([A,B,C,D,E],[A=1,B=1],[E=0,D=B,D>=0,C=0,E=0,A=C+1,D=1,A>B])



.

go(Xs,C1,C2) :-
	start_ppl,
    write('interpolant of '), write(C1), write(' and '), write(C2), write(' is: '),
	computeInterpolant(Xs,C1,C2,C3),
	ppl_finalize,
	 write(C3),nl.

/*
go:-
    numbervars([A,B,C,D], 0,_),
    start_ppl,
    interpolant:computeInterpolant([A,B,C,D],[D>100,B=D-10],[C>100,D=C-10,A=<100,C=A+11,A=<100,B>91],Ints),
    %interpolant:computeInterpolant([A,B],[B-A=10],[B>90, B=<101,A>91],Ints),
    %interpolant:computeInterpolant([A,B],[-1*B+1*A=< -10,B+ -1*A=<10],[-1*B< -90,B=<101,-1*A< -91],Ints),
    %interpolant:computeInterpolant([A,B],[A-B=10],[A-B<10],Ints),
    %interpolant:computeInterpolant([B,D],[-D < -100, B-D=< -10, -B+D =< 10],[-D > -90, D=<101, -B< -91],Ints),
    write('Interpolant is '), write(Ints), nl.
*/

% if the first constraint is unsat then the interpolant is false
computeInterpolant(_,A,_,[0=1]) :-
    \+satisfiable(A,_),
	!.
% if the second constraint is unsat then the interpolant is true
computeInterpolant(_,_,B,[1=1]) :-
    \+satisfiable(B,_),
	!.
computeInterpolant(_,SCs,CCs,[1=0]) :-
	append(SCs,CCs,Cs),
	satisfiable(Cs,_),
    %write(Cs), nl,
    %write('the constraints for interpolation are satisfiable '),nl,
	!.


computeInterpolant(Xs,SCs,CCs,[I]) :-
    makePolyhedron(SCs, P1),
    getConstraint(P1, A),
    makePolyhedron(CCs, P2),
    getConstraint(P2, B),
	interpolant(Xs,A,B,I).

interpolant(Vs,C1,C2,C3) :-
	length(Vs,N),
	normalizeConstraints(C1, Vs, C1N),
    %write(C1N),nl,
	normalizeConstraints(C2, Vs, C2N),
    %write(C2N),nl,
	coeffs(C1N,Vs,As,A,ALt),
	coeffs(C2N,Vs,Bs,B,BLt),
	length(As,MA),
	length(Bs,MB),
	varList(MA,Lambda),
	varList(MB,Mu),
	varList(N,I),
	columnVector(N,Vs,X),
	zeros(MA,ZeroA),
	zeros(MB,ZeroB),
	zeros(N,ZeroN),
	selectRows(ALt,Lambda,LambdaLt),
	selectRows(BLt,Mu,MuLt),
    varset((I,Delta,Lambda,Mu), VsMatrix),
	numbervars((I,Delta,Lambda,Mu),N,_),
    %write('I '), write(I), nl,
    %write('lambda '), write(Lambda), nl,
    %write('Mu '), write(Mu),nl,
    %write('lambda '), write(Delta),nl,
	genConstraints([
			[Lambda]>=[ZeroA],
			[Mu]>=[ZeroB],
			[Lambda]*As=[I],
			[[Delta]]=[Lambda]*A,
			[Lambda]*As+[Mu]*Bs=[ZeroN]
		],Phi,[]),
    yices_init,
	makeInterpolant(VsMatrix, Phi,X,I,Delta,Lambda,Mu,A,B,LambdaLt,MuLt,C3),
    yices_exit,
	!.

makeInterpolant(VsMatrix, Phi,X,I,Delta,Lambda,Mu,A,B,_,_,C) :-
	genConstraints([
			%[Lambda]*A + [Mu]*B < [[0]]
            [Lambda]*A + [Mu]*B =< [[-1]]
		],Phi1,[]),
	append(Phi1,Phi,Psi),
    makeRealVars(VsMatrix, VReals),
    yices_model(Psi,VReals,Model),
	!,
    write('Case 1'),nl,
    getValuesI(Model, [Delta|I], [ValueDelta|ValuesI]),
    enumerateTerm([ValuesI]*X,[[Ts]]),
    C3 =.. ['=<',Ts,ValueDelta],
	simplifyInterpolant(C3,C).
/*
makeInterpolant(VsMatrix, Phi,X,I,Delta,Lambda,Mu,A,B,[],_,C) :-
	genConstraints([
			[Lambda]*A + [Mu]*B =< [[0]]
		],Phi1,[]),
	append(Phi1,Phi,Psi),
    makeRealVars(VsMatrix, VReals),
    yices_model(Psi,VReals,Model),
	!,
	write('Case 2.1'),nl,
	getValuesI(Model, [Delta|I], [ValueDelta|ValuesI]),
    enumerateTerm([ValuesI]*X,[[Ts]]),
    C3 =.. ['=<',Ts,ValueDelta],
	simplifyInterpolant(C3,C).
*/

makeInterpolant(VsMatrix, Phi,X,I,Delta,Lambda,Mu,A,B,LambdaLt,MuLt,C) :-
	genConstraints([
			[Lambda]*A + [Mu]*B =< [[0]]
		],Phi1,[]),
    append(Phi,Phi1,Psi),
    makeInterpolant2_3(VsMatrix, Psi,X,I,Delta,LambdaLt,MuLt,C).

makeInterpolant2_3(VsMatrix, Phi,X,I,Delta,LambdaLt,_,C) :-
    length(LambdaLt, SizeLambdaLt),
    SizeLambdaLt>0,
    zeros(SizeLambdaLt,ZeroLambdaLt),
	genConstraints([[LambdaLt] > [ZeroLambdaLt]], Phi2, []),
    list2Disj(Phi2, Phi2Disj),
    append(Phi,[Phi2Disj],Psi),
    makeRealVars(VsMatrix, VReals),
    yices_model(Psi,VReals,Model),
    write('Case 2'),nl,
    !,
    getValuesI(Model, [Delta|I], [ValueDelta|ValuesI]),
    enumerateTerm([ValuesI]*X,[[Ts]]),
    C3 =.. ['<',Ts,ValueDelta],
	simplifyInterpolant(C3,C).

makeInterpolant2_3(VsMatrix, Phi,X,I,Delta,_,MuLt,C) :-
    length(MuLt, SizeMuLt),
    %SizeMuLt>0,
    zeros(SizeMuLt,ZeroMuLt),
    genConstraints([[MuLt] > [ZeroMuLt]], Phi2, []),
    list2Disj(Phi2, Phi2Disj),
    append(Phi,[Phi2Disj],Psi),
    makeRealVars(VsMatrix, VReals),
    yices_model(Psi,VReals,Model),
    write('Case 3'),nl,
    !,
    getValuesI(Model, [Delta|I], [ValueDelta|ValuesI]),
    enumerateTerm([ValuesI]*X,[[Ts]]),
    C3 =.. ['=<',Ts,ValueDelta],
	simplifyInterpolant(C3,C).



getIntValueTerm(Model, Term, IntValue):-
	get_value_as_term(Model, Term, Value),
    yices_term_to_string(Value, 20,1, 0, Value2), %converting yices term to string
    %write_string(Value2), nl,
    %changes sting code to atoms
    stringToNum( Value2, IntValue).

getValuesI(_, [], []).
getValuesI(Model, [L|Lambdas], [LV|LambdaValues]):-
    getIntValueTerm(Model, L, LV),
    getValuesI(Model, Lambdas, LambdaValues).

varList(0,[]).
varList(N,[_|Xs]) :-
	N>0,
	N1 is N-1,
	varList(N1,Xs).
	
zeros(0,[]).
zeros(N,[0|Xs]) :-
	N>0,
	N1 is N-1,
	zeros(N1,Xs).
	
columnVector(0,[],[]).
columnVector(N,[V|Vs],[[V]|Xs]) :-
	N>0,
	N1 is N-1,
	columnVector(N1,Vs,Xs).
	
selectRows([],[],[]).
selectRows([null|ALt],[_|Xs],Ys) :-
	!,
	selectRows(ALt,Xs,Ys).
selectRows([_|ALt],[L|Xs],[L|Ys]) :-
	selectRows(ALt,Xs,Ys).
	
	
coeffs([],_,[],[],[]).
coeffs([T1=<T2|Cs],Xs,[Coeffs|As],[[Const]|A],[null|ALt]) :-
	varCoeffs(Xs,T1,T2,Coeffs),
	const(T1,T2,Const),
	coeffs(Cs,Xs,As,A,ALt).
coeffs([T1<T2|Cs],Xs,[Coeffs|As],[[Const]|A],[[Const]|ALt]) :-
	varCoeffs(Xs,T1,T2,Coeffs),
	const(T1,T2,Const),
	coeffs(Cs,Xs,As,A,ALt).
coeffs([T1>=T2|Cs],Xs,As,A,ALt) :-
	coeffs([T2=<T1|Cs],Xs,As,A,ALt).
coeffs([T1>T2|Cs],Xs,As,A,ALt) :-
	coeffs([T2<T1|Cs],Xs,As,A,ALt).
coeffs([T1=T2|Cs],Xs,As,A,ALt) :-
	coeffs([T2=<T1, T1=<T2|Cs],Xs,As,A,ALt).
	
varCoeffs([X|Xs],T1,T2,[C|Coeffs]) :-
	varOccurs(T1,X,C),
	!,
	varCoeffs(Xs,T1,T2,Coeffs).
varCoeffs([X|Xs],T1,T2,[D|Coeffs]) :-
	varOccurs(T2,X,C),
	!,
	D is -C,
	varCoeffs(Xs,T1,T2,Coeffs).
varCoeffs([_|Xs],T1,T2,[0|Coeffs]) :-
	varCoeffs(Xs,T1,T2,Coeffs).
varCoeffs([],_,_,[]).

varOccurs(X+_,X,1) :-
	!.
varOccurs(A*X,X,A) :-
	!.
varOccurs(A*X+_,X,A) :-
	!.	
varOccurs(T1+T2,X,A) :-
(varOccurs(T1,X,A); varOccurs(T2,X,A)).
varOccurs(X,X,1) :-
	!.
const(T1,_,D) :-
	constOccur(T1,C),
	!,
	D is -C.
const(_,T2,D) :-
	constOccur(T2,C),
	!,
	D is C.
const(_,_,0).
	
constOccur(C,C) :-
	number(C),
	!.
constOccur(C+_,C) :-
	number(C),
	!.
constOccur(_+T,C) :-
	constOccur(T,C).


genConstraints([],Es,Es).
genConstraints([C|Cs],Es0,Es2) :-
	genConstraints(C,Es0,Es1),
	genConstraints(Cs,Es1,Es2).
genConstraints(C,Es0,Es1) :-
	C=..[F,T1,T2],
	%member(F,['=','>','<','=<','>=', '=\=']),
    member(F,[=,>,<,=<,>=, =\=]),
	enumerateTerm(T1,Ts1),
	enumerateTerm(T2,Ts2),
	genConstraintList(Ts1,Ts2,F,Es0,Es1).
	
genConstraintList([],[],_,Es,Es).

genConstraintList([T1|Ts1],[T2|Ts2],F,[(C1;C2)|Es0],Es1) :-
	nonlist(T1),
    F= =\=,
	!,
	C1=..[>,T1,T2],
    C2=..[<,T1,T2],
	genConstraintList(Ts1,Ts2,F,Es0,Es1).
/*
genConstraintList([T1|Ts1],[T2|Ts2],F,[(C1;C2)|Es0],Es1) :-
	nonlist(T1),
    F= =\=,
	!,
    %using very specific information over integers
    T2GE is T2+1,
    T2LE is T2-1,
	C1=..[>=,T1,T2GE],
    C2=..[=<,T1,T2LE],
	genConstraintList(Ts1,Ts2,F,Es0,Es1).
*/
genConstraintList([T1|Ts1],[T2|Ts2],F,[C|Es0],Es1) :-
	nonlist(T1),
	!,
	C=..[F,T1,T2],
	genConstraintList(Ts1,Ts2,F,Es0,Es1).
genConstraintList([T1|Ts1],[T2|Ts2],F,Es0,Es2) :-
	genConstraintList(T1,T2,F,Es0,Es1),
	genConstraintList(Ts1,Ts2,F,Es1,Es2).
	
nonlist(T) :-
	var(T),
	!.
nonlist(T) :-
	functor(T,F,N),
	F/N \== '.'/2,
	F/N \== []/0.


enumerateTerm(T1+T2,Ts) :-
	!,
	enumerateTerm(T1,Ts1),
	enumerateTerm(T2,Ts2),
	sum(Ts1,Ts2,Ts).
enumerateTerm(T1*T2,Ts) :-
	!,
	product(T1,T2,Ts).
enumerateTerm(T1,T1).
	
product([Row|Rows],M,[P|Ps]) :-
	rowProduct(M,Row,P),
	product(Rows,M,Ps).
product([],_,[]).

rowProduct(M,_,[]) :-
	nullRows(M),
	!.
rowProduct(M,Row,[T|Ts0]) :-
	nextCol(M,Col,M1),
	vectorProduct(Row,Col,T),
	rowProduct(M1,Row,Ts0).
	
nextCol([],[],[]).
nextCol([[X|Xs]|M],[X|Col],[Xs|M1]) :-
	nextCol(M,Col,M1).
	
vectorProduct([],[],0).
vectorProduct([X],[Y],X*Y).
vectorProduct([X1,X2|Xs],[Y1,Y2|Ys],X1*Y1+T) :-
	vectorProduct([X2|Xs],[Y2|Ys],T).

nullRows([]).
nullRows([[]|N]) :-
	nullRows(N).
	
sum([],[],[]).
sum([Row1|Ts1],[Row2|Ts2],[Row|Ts]) :-
	vectorSum(Row1,Row2,Row),
	sum(Ts1,Ts2,Ts).
	
vectorSum([],[],[]).
vectorSum([X|Xs],[Y|Ys],[X+Y|Sum]) :-
	vectorSum(Xs,Ys,Sum).


simplifyInterpolantConstr(E1,E2) :-
	E1 =.. [Op,T1,T2],
	simplifyTerm(T1,T11),
	E2 =.. [Op,T11,T2].
	
simplifyTerm(C3,0) :-
	allZeroCoeffs(C3),
	!.
simplifyTerm(1*X, X) :-
	!.
simplifyTerm(0*_, 0) :-
	!.
simplifyTerm(D*X, D*X) :-
	!.
simplifyTerm(1*X+C3,C) :-
	!,
	simplifyTerm(C3,C2),
	(C2=0 -> C=X; C=X+C2).
simplifyTerm(0*_+C3,C) :-
	!,
	simplifyTerm(C3,C).
simplifyTerm(D*X+C3,C) :-
	!,
	simplifyTerm(C3,C2),
	(C2=0 -> C=D*X; C=D*X+C2).
	
allZeroCoeffs(0*_).
allZeroCoeffs(0*_+C) :-
	allZeroCoeffs(C).

%making real yices variables
makeRealVars([], []).
makeRealVars([V|Vs], [(V,real)|VReals]):-
    makeRealVars(Vs, VReals).

list2Disj([A], (A)):-
    !.
list2Disj([A|R], ((A);R1)):-
    !,
    list2Disj(R, R1).
list2Disj([], (1=1)).


simplifyInterpolant(E1, E3):-
    simplifyInterpolantConstr(E1, E2),
    transf(E2,E3).


stringToNum(S,T):-
    read_from_string(S,T).

