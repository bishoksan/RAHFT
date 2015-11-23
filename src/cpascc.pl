:- module(cpascc,_).

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
			
main(['-prg',FileIn]) :-
	!,
	go(FileIn).		
main(['-prg',FileIn, '-o', FileOut]) :-
	!,
	go2(FileIn,FileOut).
main(ArgV) :-
	write('Starting analysis'),nl,
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
	nl, write('Analysis Succeeded'),nl,
	end_time(user_output),
	!,
	factFile(FactFile),
	ppl_finalize.

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Iterate solves each component 
% recursive components involve iterative fixpoint
% non-recursive components solved in one step.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

iterate([(non_recursive,P)|SCCs]) :-
	verbose_write(['Non-recursive component ',P]),
	(flag(first) -> true; assert(flag(first))),
	non_recursive_scc(P),
	iterate(SCCs).
iterate([(recursive,Ps)|SCCs]) :-
	verbose_write(['Recursive component ',Ps]),
	(flag(first) -> true; assert(flag(first))),
	recursive_scc(Ps),
	iterate(SCCs).
iterate([]).

non_recursive_scc(P) :-
	convexhull_operation(P),
	retract(operatorcount(X)),
	Y is X + 1,
	assert(operatorcount(Y)),
	write('-'),write(X),
	newoldfacts,
	switch_flags.
	
recursive_scc(Ps) :-
	convexhull_operation(Ps),
	retract(operatorcount(X)),
	Y is X + 1,
	assert(operatorcount(Y)),
	write('-'),write(X),
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

% adding constraints for missing dimensions
dummyCList([],[]).
dummyCList([C|Cs],[C-C=0|Cs1]) :-
	dummyCList(Cs,Cs1).

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
	assert(currentflag(F/N)),
	fail.
switch_flags :-
	true.

isflagset(F) :-
    functor(F,Fn,N),
    currentflag(Fn/N).

raise_flag(F):-
    functor(F,Fn,N),
	(nextflag(Fn/N) ->
	true;  
	assert(nextflag(Fn/N))
	).

record(F,H) :-
	cond_assert(F,H).
	
narrow_record(F,H) :- 
	narrow_cond_assert(F,H).

cond_assert(F,H):-
	\+ (fact(F,H1), entails(H1,H)),
	getExistingConstraints(F,H0),
	convhull(H0,H,H2),
	assert(newfact(F,H2)),
	raise_flag(F).
	%check_raise_flag(F,H0,H2).

narrow_cond_assert(F,H):-
	\+ (newfact(F,H1), entails(H1,H)),
	getExistingNewConstraints(F,H0),
	convhull(H0,H,H2),
	retractall(newfact(F,_)),
	assert(newfact(F,H2)).
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
	assert(oldfact(F,H)),
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
	assert(widening_point(WcF/WcN,P,ND)),
	widenlist(Ws).
widenlist([Wc|Ws]) :-
	functor(Wc,WcF,WcN),
    widening_point(WcF/WcN,_,0),
    retract(newfact(Wc,NewH)),
    retract(oldfact(Wc,OldH)),
    verbose_write(['Widening at ',Wc]),
    wutwiden(Wc,NewH,OldH,H2),
    assert(oldfact(Wc,H2)),
	widenlist(Ws).

wutwiden(F,H0,H1,H2) :-
    widenWRToptions(F,H0,H1),
	H2 = H0,
	(equivalent(H1,H2) -> true;
		raise_flag(F)).

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
	widenf(bhrz03) -> widenPolyhedraBHRZ03(H0,H1);
		widenPolyhedraH79(H0,H1).
		
widenUpto(H0,H1,Cs) :-
	widenf(bhrz03) -> widenUptoBHRZ03(H0,H1,Cs);
		widenUptoH79(H0,H1,Cs).

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

readWutfacts :-
	open('wut.props',read,S),
	read(S,C),
	assertWutFacts(C,S),
	close(S).
	
assertWutFacts(end_of_file,_) :-
	!.
assertWutFacts((H :- C), S) :-
	numbervars((H :- C),0,_),
	assert(invariant(H,C)),
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
	assert(prio_widen(RWps)).
	
assertWP(widening_point(X,Y)) :-
	!,
	delays(D),
	assert(widening_point(X,Y,D)).
assertWP(widening_point(X)) :-
	!,
	delays(D),
	assert(widening_point(X,0,D)).
	
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

% get_options/3 provided by Michael Leuschel
get_options([],[],[]).
get_options([X|T],Options,Args) :-
   (recognised_option(X,Opt,Values) ->
	 ( append(Values, Rest, T),
	 RT = Rest,
	 Options = [Opt|OT], Args = AT
	 )
   ;
	 (
	 Options = OT,	Args = [X|AT],
	 RT = T
	 )
   ),
   get_options(RT,OT,AT).

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
	
set_options(Options,File,FactFile) :-
	member(programO(File),Options),
	(member(verbose,Options) -> assert(flag(verbose));
		retractall(flag(verbose))),
	(member(singlepoint,Options) -> assert(widenAt(singlepoint));
		assert(widenAt(allpoints))),
	(member(widenO(WOutput),Options) -> true;
		WOutput='widencns'),
	(member(widenF(WFunc),Options) -> assert(widenf(WFunc));
		assert(widenf(h79))),
	(member(detectwps(M),Options) -> assert(detectwps(M));
		assert(detectwps(feedback))),
	(member(withwut,Options) -> 
			assert(widenf(withwut)),readWutfacts,
			(flag(verbose) -> write('Widening points: '),nl,showallwideningpoints;
				true);
		assert(widenf(nowut))),
	(member(widenP(WPoints),Options) -> true;
		WPoints='widenpoints'),
	(member(narrowO(NOutput),Options) -> true;
		NOutput='stdnarrowout'),
	(member(factFile(FactFile),Options) -> true;
		FactFile='chaFacts'),
	(member(narrowiterationsO(Nit),Options) -> atom_number(Nit,NitN);
		NitN is 0),
	(member(delaywiden(DWit),Options) -> atom_number(DWit,DWitN);
		DWitN is 0),
	assert(delays(DWitN)),
	assert(narrowiterations(NitN)),
	detectwps(WPSMethod),
	(member(nowpscalc,Options) -> true;
		wto_file(File,WPSMethod,WPoints)),
	load_widenpoints(WPoints),
	assert(outputfile(WOutput)).
	
%%%% clean workspace

initialise :-
	assert(operatorcount(0)),
	assert(flag(first)).

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


factFile(File) :-
	(File=user_output -> Sout=user_output; open(File,write,Sout)),
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
	flag(verbose) -> verbose_write_list(Xs);
		true.
		
verbose_write_list([]) :-
	nl.
verbose_write_list([X|Xs]) :-
	write(X),
	verbose_write_list(Xs).
	
