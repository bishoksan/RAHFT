:- module(cpascc, [main/1], []).

% It also generates path FTA, modified by Bish on 21-01-2016

:- use_module(library(read)).
:- use_module(library(write)).
:- use_module(library(aggregates)).
:- use_module(library(dynamic)).

:- use_module(setops).
:- use_module(canonical).
:- use_module(wto).
:- use_module(linearize).
:- use_module(library(terms_vars)).
:- use_module(library(ppl)).
:- use_module(library(lists)).
:- use_module(timer_ciao).
:- use_module(input_ppl).
:- use_module(ppl_ops).
:- use_module(scc).

:- include(common).

:- dynamic(flag/1).
:- dynamic(currentflag/1).
:- dynamic(nextflag/1).
:- dynamic(operatorcount/1).
:- dynamic(widening_point/3).
:- dynamic(outputfile/1).
:- dynamic(newfact/2).
:- dynamic(oldfact/2).
:- dynamic(prio_widen/1).
:- dynamic(widenAt/1).
:- dynamic(widenf/1).
:- dynamic(detectwps/1).
:- dynamic(delays/1).
:- dynamic(clauseCount/1).
:- dynamic(invariant/2).
:- dynamic(narrowiterations/1).
:- dynamic(versionCount/1).
:- dynamic(versiontransition/2).
:- dynamic(version/3).
:- dynamic(clauseCount/1).
:- dynamic(pathtransition/1).
:- dynamic(atomicproposition/1).
:- dynamic cEx/1.
:- dynamic threshold/1.

go(File) :-
	go2(File,temp).
	
go2(FileIn,FileOut) :-
	cpascc:main(
		['-prg',FileIn,
		'-widenpoints','widenpoints',
		'-widenout','widencns',
		'-narrowout','narrowcns',
		'-narrowiterations','0',
		'-delaywidening','0',
		'-withwut',
		'-wfunc','h79',
		'-o',FileOut]).
			
recognised_option('-prg',programO(R),[R]).
recognised_option('-widenpoints',widenP(R),[R]).
recognised_option('-widenout',widenO(R),[R]).
recognised_option('-narrowout',narrowO(R),[R]).
recognised_option('-narrowiterations',narrowiterationsO(R),[R]).
recognised_option('-delaywidening',delaywiden(R),[R]).
recognised_option('-wfunc',widenF(F),[F]).
recognised_option('-v',verbose,[]).
recognised_option('-querymodel',querymodel(Q),[Q]).
recognised_option('-nowpscalc',nowpscalc,[]).
recognised_option('-withwut',withwut,[]).
recognised_option('-detectwps',detectwps(M),[M]).
recognised_option('-o',factFile(F),[F]).
recognised_option('-cex',counterExample(F),[F]).
recognised_option('-threshold',thresholdFile(F),[F]).
	
main(['-prg',FileIn]) :-
	!,
	go(FileIn).		
main(['-prg',FileIn, '-o', FileOut]) :-
	!,
	go2(FileIn,FileOut).
main(ArgV) :-
	write('Starting Convex Polyhedra analysis'),nl,
	get_options(ArgV,Options,_),
	cleanWorkspace,
	set_options(Options,File,FactFile),
	initialise,
	start_time,
	load_file(File,pl),
	dependency_graph(Es,Vs),
	scc_graph(Es,Vs,G),
	start_ppl,
	iterate(G),
	narrow,
	nl, write('Convex Polyhedra Analysis Succeeded'),nl,
	end_time(user_output),
	!,
	factFile(FactFile),
	generateCEx,
	ppl_finalize.


generateCEx:-
    cEx('$NOCEX'),
    !.
generateCEx:-
    cEx(CexFile),
    buildversions2,
    versioniterate,
    open(CexFile,write,S),
    findCounterexampleTrace(S),
    close(S).

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Iterate solves each component 
% recursive components involve iterative fixpoint
% non-recursive components solved in one step.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

iterate([(non_recursive,P)|SCCs]) :-
	verbose_write(['Non-recursive component ',P]),
	(flag(first) -> true; assertz(flag(first))),
	non_recursive_scc(P),
	iterate(SCCs).
iterate([(recursive,Ps)|SCCs]) :-
	verbose_write(['Recursive component ',Ps]),
	(flag(first) -> true; assertz(flag(first))),
	recursive_scc(Ps),
	iterate(SCCs).
iterate([]).

non_recursive_scc(P) :-
	convexhull_operation(P),
	retract(operatorcount(X)),
	Y is X + 1,
	assertz(operatorcount(Y)),
	%write('-'),write(X),
	newoldfacts,
	switch_flags.
	
recursive_scc(Ps) :-
	convexhull_operation(Ps),
	retract(operatorcount(X)),
	Y is X + 1,
	assertz(operatorcount(Y)),
	%write('-'),write(X),
	retractall(flag(first)),
	fail.
recursive_scc(Ps) :-
	(flag(verbose)->factFile(user_output);
		true),
	widen,
	newoldfacts,
	not_converged, 
	!,
	switch_flags,
	recursive_scc(Ps).
recursive_scc(_).
	
not_converged :-
	nextflag(_).
	
convexhull_operation(Ps) :-
	member(P/N,Ps),
	functor(H,P,N),
	my_clause(H,B),
	operator(H,B),
	fail.
convexhull_operation(_).

	
%%% narrowing iterations %%%

narrow :-
	narrowiterations(NitN),
	narrow1(NitN).

narrow1(0).
narrow1(X):-
	X > 0,
	narrowIteration,
	newoldfacts,
	Y is X-1,
	narrow1(Y).
	
narrowIteration :- 
	my_clause(H,B),
	narrowOperator(H,B),
	fail.
narrowIteration.

%%%%%%%%%%%%%%%%

operator(Head,B):-
	(changed(B);flag(first)),
	prove(B,Cs),
	varset((Head,Cs),Ys),
	Head =.. [_|Xs],
	linearize(Cs,CsLinNOP),
	dummyCList(Xs,DCLx),
	dummyCList(Ys,DCLy),
	append(DCLx,DCLy,DCL),
	append(CsLinNOP,DCL,CsLin),
	numbervars((Head:-CsLin),0,_),
	satisfiable(CsLin,H1),
	setdiff(Ys,Xs,Zs),
	project(H1,Zs,Hp),
	record(Head,Hp).


changed(Bs) :- 
	member(B,Bs),
	isflagset(B),
	!.

prove([],[]).
prove([true],[]).
prove([B|Bs],[C|Cs]):-
	constraint(B,C),
	!,
	prove(Bs,Cs).
prove([B|Bs],Cs):-
	getoldfact(B,CsOld),
	prove(Bs,Cs2),
	append(CsOld,Cs2,Cs).

narrowOperator(Head,B):-
	prove(B,Cs),
	varset((Head,Cs),Ys),
	Head =.. [_|Xs],
	linearize(Cs,CsLinNOP),
	dummyCList(Xs,DCLx),
	dummyCList(Ys,DCLy),
	append(DCLx,DCLy,DCL),
	append(CsLinNOP,DCL,CsLin),
	numbervars((Head:-CsLin),0,_),
	satisfiable(CsLin,H1),
	setdiff(Ys,Xs,Zs),
	project(H1,Zs,Hp),
	narrow_record(Head,Hp).

%%%%%%%%%%%%%%%%%
%  new/old facts
%%%%%%%%%%%%%%%%%

switch_flags :-
	retractall(currentflag(_/_)),
	retract(nextflag(F/N)),
	assertz(currentflag(F/N)),
	fail.
switch_flags :-
	true.

isflagset(F) :-
	functor(F,Fn,N),
	currentflag(Fn/N).

raise_flag(F):-
	functor(F,Fn,N),
	( nextflag(Fn/N) ->
	    true
	; assertz(nextflag(Fn/N))
	).

record(F,H) :-
	cond_assert(F,H).
	
narrow_record(F,H) :- 
	narrow_cond_assert(F,H).

cond_assert(F,H):-
	\+ (fact(F,H1), entails(H1,H)),
	getExistingConstraints(F,H0),
	convhull(H0,H,H2),
	assertz(newfact(F,H2)),
	raise_flag(F).
	%check_raise_flag(F,H0,H2).

narrow_cond_assert(F,H):-
	\+ (newfact(F,H1), entails(H1,H)),
	getExistingNewConstraints(F,H0),
	convhull(H0,H,H2),
	retractall(newfact(F,_)),
	assertz(newfact(F,H2)).
	%check_raise_flag(F,H0,H2).

getExistingConstraints(F,H0) :-
	retract(newfact(F,H0)),
	!.
getExistingConstraints(F,H0) :-
	oldfact(F,H0),
	!.
getExistingConstraints(_,empty).

getExistingNewConstraints(F,H0) :-
	newfact(F,H0),
	!.
getExistingNewConstraints(_,empty).

check_raise_flag(F,empty,_) :-
	!,
	raise_flag(F).
check_raise_flag(_,H0,H2) :-
	equivalent(H0,H2),
	!.
check_raise_flag(F,_,_) :-
	raise_flag(F).

getoldfact(B,Cs1) :-
	functor(B,F,N),
	functor(B1,F,N),
	oldfact(B1,H),
	ppl_Polyhedron_get_minimized_constraints(H,Cs2),
	melt((B1,Cs2),(B,Cs1)).

fact(X,Y) :-
	newfact(X,Y).
fact(X,Y) :-
	oldfact(X,Y).
	
newoldfacts :-
	retract(newfact(F,H)),
	retractall(oldfact(F,_)),
	assertz(oldfact(F,H)),
	fail.
newoldfacts.

%%%%%%%%%%%%%
% Widening
%%%%%%%%%%%%%

widen :-
	prio_widen(PrioWiden),
	possibleWidenPs(PrioWiden,PosPW),
	verbose_write(['Possible Wideningpoints ',PosPW]),
	!,
	widenlist(PosPW).

possibleWidenPs([],[]).
possibleWidenPs([(Dg,F,N)|WPs],[Fn|PWPs]) :-
	widening_point(F/N,Dg,_Delays),
	functor(Fn,F,N),
	newfact(Fn,_),
	oldfact(Fn,_),
	!,
	possibleWidenPs(WPs,PWPs).
possibleWidenPs([_|WPs],PWPs) :-
	!,
	possibleWidenPs(WPs,PWPs).

widenlist([]).
widenlist([Wc|Ws]) :-
	functor(Wc,WcF,WcN),
	widening_point(WcF/WcN,P,D), % delay widening
	D > 0,
	!,
	ND is D-1,
	retractall(widening_point(WcF/WcN,P,D)),
	assertz(widening_point(WcF/WcN,P,ND)),
	widenlist(Ws).
widenlist([Wc|Ws]) :-
	functor(Wc,WcF,WcN),
	widening_point(WcF/WcN,_,0),
	retract(newfact(Wc,NewH)),
	retract(oldfact(Wc,OldH)),
	verbose_write(['Widening at ',Wc]),
	wutwiden(Wc,NewH,OldH,H2),
	assertz(oldfact(Wc,H2)),
	widenlist(Ws).

wutwiden(F,H0,H1,H2) :-
	widenWRToptions(F,H0,H1),
	H2 = H0,
	( equivalent(H1,H2) ->
	    true
	; raise_flag(F)
	).

widenWRToptions(_,H0,H1) :-
	widenf(nowut),
	!,
	widenPolyhedra(H0,H1).
widenWRToptions(F,H0,H1) :-
	widenf(withwut),
	!,
	getThresholds(F,Cns),
	verbose_write(['Widen upto constraints: ',Cns]),
	widenUpto(H0,H1,Cns).

widenPolyhedra(H0,H1) :-
	( widenf(bhrz03) -> widenPolyhedraBHRZ03(H0,H1)
	; widenPolyhedraH79(H0,H1)
	).
		
widenUpto(H0,H1,Cs) :-
	( widenf(bhrz03) -> widenUptoBHRZ03(H0,H1,Cs)
	; widenUptoH79(H0,H1,Cs)
	).

getThresholds(F,Cout) :-
	bagof(Cs,invariant(F,Cs),Clist),
	!,
	flattenList(Clist,Cout).
getThresholds(_,[]).

flattenList([],[]).
flattenList([L|Ls],Lout) :-
	flattenList(Ls,Lpre),
	append(L,Lpre,Lout).
	
%%% input threshold constraints %%%%
readWutfacts:-
	threshold('$NOTHRESHOLD'),
	!.
readWutfacts :-
	threshold(TFile),
	open(TFile,read,S),
	read(S,C),
	assertWutFacts(C,S),
	close(S).
	
assertWutFacts(end_of_file,_) :-
	!.
assertWutFacts((H :- C), S) :-
	numbervars((H :- C),0,_),
	assertz(invariant(H,C)),
	read(S,C1),
	assertWutFacts(C1,S).
	
%%% input widening points %%%%
	
load_widenpoints(WPfile) :-
	open(WPfile,read,WP),
	read(WP,W),
	assert_widenpoints(W, WP),
	collect_wps.
	
assert_widenpoints(end_of_file,WP) :-
	!,
	close(WP).
assert_widenpoints(W,WP) :-
	assertWP(W),
	read(WP,W1),
	assert_widenpoints(W1,WP).
	
collect_wps :-
	findall((Dgs,F,N),widening_point(F/N,Dgs,_Delays),Wps),
	reverse(Wps,RWps),
	verbose_write(['Ordered by degree ',RWps]),
	assertz(prio_widen(RWps)).
	
assertWP(widening_point(X,Y)) :-
	!,
	delays(D),
	assertz(widening_point(X,Y,D)).
assertWP(widening_point(X)) :-
	!,
	delays(D),
	assertz(widening_point(X,0,D)).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicate dependency graph
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dependency_graph(Es,Vs) :-
	findall(P/N-Q/M, (
			my_clause(H,Bs),
			functor(H,P,N),
			member(B,Bs),
			\+ constraint(B,_),
			functor(B,Q,M)
			),
			Es),
	findall(A, (
			member(X-Y,Es),
			(A=X; A=Y)
			),
			Vs1),
	findall(P/N, (my_clause(H,_),functor(H,P,N)), Vs2),
	append(Vs1,Vs2,Vs).		
	

%%%% Getting and setting options

set_options(Options,File,FactFile) :-
	member(programO(File),Options),
	( member(verbose,Options) -> assertz(flag(verbose))
	; retractall(flag(verbose))
	),
	( member(singlepoint,Options) -> assertz(widenAt(singlepoint))
	; assertz(widenAt(allpoints))
	),
	( member(widenO(WOutput),Options) -> true
	; WOutput='widencns'
	),
	( member(widenF(WFunc),Options) -> assertz(widenf(WFunc))
	; assertz(widenf(h79))
	),
	( member(detectwps(M),Options) -> assertz(detectwps(M))
	; assertz(detectwps(feedback))
	),
	( member(thresholdFile(TFile),Options) -> assertz(threshold(TFile))
	; assertz(threshold('$NOTHRESHOLD'))
	),
	( member(withwut,Options) ->
	  assertz(widenf(withwut)),
	  readWutfacts,
	  ( flag(verbose) ->
	      write('Widening points: '),nl,
	      showallwideningpoints
	  ; true
	  )
	; assertz(widenf(nowut))
	),
	( member(widenP(WPoints),Options) -> true
	; WPoints='widenpoints'
	),
	( member(narrowO(NOutput),Options) -> true
	; NOutput='stdnarrowout'
	),
	( member(factFile(FactFile),Options) -> true
	; true
	),
	( member(narrowiterationsO(Nit),Options) -> atom_number(Nit,NitN)
	; NitN is 0
	),
	( member(delaywiden(DWit),Options) -> atom_number(DWit,DWitN)
	; DWitN is 0
	),
	( member(counterExample(CexFile),Options) -> assertz(cEx(CexFile))
	; assertz(cEx('$NOCEX'))
	),
	assertz(delays(DWitN)),
	assertz(narrowiterations(NitN)),
	detectwps(WPSMethod),
	( member(nowpscalc,Options) -> true
	; wto_file(File,WPSMethod,WPoints)
	),
	load_widenpoints(WPoints),
	assertz(outputfile(WOutput)).
	
%%%% clean workspace

initialise :-
	assertz(operatorcount(0)),
	assertz(flag(first)).

cleanWorkspace :-
	retractall(flag(_)),
	retractall(currentflag(_)),
	retractall(nextflag(_)),
	retractall(operatorcount(_)),
	retractall(widening_point(_,_,_)),
	retractall(outputfile(_)),
	retractall(newfact(_,_)),
	retractall(oldfact(_,_)),
	retractall(prio_widen(_)),
	retractall(widenAt(_)),
	retractall(widenf(_)),
	retractall(detectwps(_)),
	retractall(delays(_)),
	retractall(clauseCount(_)),
	retractall(versionCount(_)),
	retractall(versiontransition(_,_)),
	retractall(version(_,_,_)),
	retractall(pathtransition(_)),
	retractall(atomicproposition(_)),
	retractall(cEx(_)),
	retractall(narrowiterations(_)).
	
%%%% Output 

showallwideningpoints:-
	widening_point(X,Degree,Delays),
	write('  '),
	write(X),
	write(' Included in '),
	write(Degree),
	write(' program loops'),
	write(' - should be delayed for iterations = '),
	write(Delays),nl,
	fail.
showallwideningpoints.

factFile(user_output):-
	!.

factFile(File) :-
	open(File,write,Sout),
	%(File=user_output -> Sout=user_output; open(File,write,Sout)),
	(oldfact(F,H),
	ppl_Polyhedron_get_minimized_constraints(H,C),
	numbervars(F,0,_),
	writeq(Sout,F), write(Sout,' :- '), 
	writeq(Sout,C),
	write(Sout,'.'),
	nl(Sout),
	fail;
	close(Sout)).

verbose_write(Xs) :-
	( flag(verbose) -> verbose_write_list(Xs)
	; true
	).
		
verbose_write_list([]) :-
	nl.
verbose_write_list([X|Xs]) :-
	write(X),
	verbose_write_list(Xs).

% Version generation and FTA construction

fact3(F,H,_) :-
	oldfact(F,H).

buildversions2 :-
	assertz(versionCount(0)),
	fact3(F,H,_),
	retract(versionCount(N1)),
	N is N1+1,
	assertz(versionCount(N)),
	assertz(version(F,H,N)),
	fail.
buildversions2.

versioniterate :-
	assertz(clauseCount(0)),
	versionoperator,
	fail.
versioniterate.

versionoperator :-
	my_clause(Head,B),
	retract(clauseCount(K)),
	K1 is K+1,
	assertz(clauseCount(K1)),
	versionprove(B,Cs,Ds,Vs),
	Head =.. [_|Xs],
	linearize(Cs,CsLin),
	append(CsLin,Ds,CsDs),
	varset((Head,CsDs),Ys),
	dummyCList(Ys,DCL),
	append(CsDs,DCL,CsL),
	numbervars((Head:-CsL,Vs),0,_),
	satisfiable(CsL,H1),
	setdiff(Ys,Xs,Zs),
	project(H1,Zs,Hp),
	headversion(Head,Hp,Hv),
	assertTransition(Hv,Vs,K1).

versionprove([],[],[],[]).
versionprove([true],[],[],[true]).
versionprove([B|Bs],[C|Cs],Ds,[B|Vs]):-
	constraint(B,C),
	!,
	versionprove(Bs,Cs,Ds,Vs).
versionprove([B|Bs],Cs,Ds,[V|Vs]):-
	getversionfact(B,CsOld,V),
	versionprove(Bs,Cs,Ds1,Vs),
	append(CsOld,Ds1,Ds).
	
getversionfact(B,Cs1,Bk) :-
	functor(B,F,N),
	functor(B1,F,N),
	version(B1,H,K),	
	ppl_Polyhedron_get_minimized_constraints(H,Cs2),
	melt((B1,Cs2),(B,Cs1)),
	name(F,NF),
	name(K,NK),
	append("_v",NK,SuffK),
	append(NF,SuffK,NFK),
	name(FK,NFK),
	B =.. [F|Xs],
	Bk =.. [FK|Xs].

headversion(Head,_,Hk) :-
	version(Head,_,K), 
	Head =.. [F|Xs],
	name(F,NF),
	name(K,NK),
	append("_v",NK,SuffK),
	append(NF,SuffK,NFK),
	name(FK,NFK),
	Hk =.. [FK|Xs].

stateSymb(H,R) :-
	functor(H,F,_),
	name(F,T),
	append(_,[95|Xs],T),
	\+ member(95,Xs),
	name(R,Xs).

unaryBody([V],[X],VX) :-
	!,
	VX =.. [V,X].
unaryBody([V|Vs],[X|Xs],(VX,VXs)) :-
	VX =.. [V,X],
	unaryBody(Vs,Xs,VXs).
unaryBody([],[],true).

bodyStates([],[]) :-
	!.
bodyStates([B|Bs],BSs) :-
	constraint(B,_),
	!,
	bodyStates(Bs,BSs).
bodyStates([B|Bs],[BS|BSs]) :-
	stateSymb(B,BS),
	bodyStates(Bs,BSs).

clauseFunctor(N1,F) :-
	name(N1,M),
	append("c",M,CM),
	name(F,CM).

makeAtomicPropositionFact(true,Head,Prop) :-
	!,
	Head =.. [R|_],
	Prop =.. [prop,R,R].
makeAtomicPropositionFact(Body,_Head,Prop) :-
	Body =.. [R1,_X],
	Prop =.. [prop,R1,R1].

makeBpath(true, true) :-
	!.
makeBpath(Body,BPath) :-
	Body =.. [R1,X],
	BPath =.. [path,[R1|X]].

 makeHpath(true,Head,HPath) :-
        !,
        Head =.. [R|_],
        HPath =.. [initState,R].
 makeHpath(Body,Head,HPath) :-
	Body =.. [R1,_X],
	Head =.. [R,_],
	HPath =.. [trans,R1,R].

assertTransition(Hv,Vs,K1) :-
	stateSymb(Hv,R),
	bodyStates(Vs,BSs),
	clauseFunctor(K1,F),
	L =.. [F|BSs],
	functor(L,F,M),
	functor(L1,F,M),
	L1 =.. [F|Xs],
	canonical(L1),
	Head =.. [R,L1],
	unaryBody(BSs,Xs,Body),
	assertz(versiontransition(Head,Body)),
	makeHpath(Body,Head,HPath),
	makeBpath(Body,_BPath),
	makeAtomicPropositionFact(Body,Head,Prop),
	assertz(pathtransition(HPath)),
	assertz(atomicproposition(Prop)).

findCounterexampleTrace(S) :-
	version(false,_,Y),
	operatorcount(J),
	name(Y,K),
	append("v",K,VK),
	name(VN,VK),
	findnsols(1,X,(
		Goal =.. [VN,X],
		findTrace(Goal,J),
		write(S,counterexample(X)),
		write(S,'.'),
		nl(S),
		write(user_output,X),
		write(user_output,'.'),
		nl(user_output)),
	[_|_]).
findCounterexampleTrace(S) :-
	write(S,'safe'),
	write(S,'.'),
	nl(S).	
	
findTrace(true,_).
findTrace(Goal,J) :-
	J > 0,
	functor(Goal,P,M),
	functor(H,P,M),
	versiontransition(H,B),
	melt((H,B),(Goal,Body)),
	J1 is J-1,
	findTrace(Body,J1).
findTrace((G,Gs),J) :-
	J > 0,
	findTrace(G,J),
	findTrace(Gs,J).

for(Low,Low,High) :-
	Low =< High.
for(I,Low,High) :-
	Low < High,
	Low1 is Low+1,
	for(I,Low1,High).
	
