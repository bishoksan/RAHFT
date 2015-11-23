:- module(cha,_).

:- use_module(duplVar).
:- use_module(readprog).
:- use_module(builtins).
:- use_module(myterms).
:- use_module(setops).
:- use_module(canonical).
:- use_module(wto).
:- use_module(xmlize).
:- use_module(library(terms_vars)).

:- use_module(library(ppl)).
:- use_module(library(lists)).
:- use_module(timer_ciao).



%Modules specific for PIC/Prolog/PPL
:- use_module(input_ppl).


:- dynamic(flag/1).
:- dynamic(currentflag/1).
:- dynamic(nextflag/1).
:- dynamic(operatorcount/1).
:- dynamic(widening_point/3).
:- dynamic(outputfile/1).
:- dynamic(newfact/2).
:- dynamic(oldfact/2).
:- dynamic(wfact/2).
:- dynamic(wutInitFact/2).
:- dynamic(wutfact/2).
:- dynamic(pnfact/2).
:- dynamic(prio_widen/1).
:- dynamic(widenAt/1).
:- dynamic(widenf/1).
:- dynamic(thistoken/1).
:- dynamic(stripqueries/1).
:- dynamic(supquery/1).
:- dynamic(detectwps/1).
:- dynamic(delays/1).
:- dynamic(wdelayfor/1).
:- dynamic(versionCount/1).
:- dynamic(version/3).
:- dynamic(clauseCount/1).
:- dynamic(ftaCount/1).
:- dynamic(pathtransition/1).
:- dynamic(atomicproposition/1).
:- dynamic(versiontransition/2).
:- dynamic(invariant/2).

% 
% For debugging only - 
%


dbwrite(_).
dbnl.

%
% Remember: Introducing new variables - possibly add constraints.
%

% sample query
%?- main(['-prg','Tests/applen.pl','-widenpoints','widenpoints','-widenout','widencns','-narrowout','narrowcns','-narrowiterations','0','-delaywidening','0','-withwut','bounded','-wfunc','h79']).

go(File) :-
	go2(File,temp).
	
go2(FileIn,FileOut) :-
		cha:main(['-prg',FileIn,'-widenpoints','widenpoints','-widenout','widencns','-narrowout','narrowcns','-narrowiterations','0','-delaywidening','0','-withwut','bounded','-wfunc','h79','-o',FileOut]).
		
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
     member(programO(File),Options),
     (member(verbose,Options),assert(flag(verbose));retractall(flag(verbose))),
     (member(pic,Options),IType=pic;IType=pl),
     (member(singlepoint,Options),assert(widenAt(singlepoint));assert(widenAt(allpoints))),
     (member(querymodel(Q),Options),assert(stripqueries(yes)),assert(supquery(Q));assert(stripqueries(no))),
     (member(widenO(WOutput),Options);WOutput='widencns'),
     (member(widenF(WFunc),Options),assert(widenf(WFunc));assert(widenf(h79))),
     (member(detectwps(M),Options),assert(detectwps(M));assert(detectwps(feedback))),
     (member(withwut(WutType),Options),assert(widenf(withwut)),assert(widenf(WutType)),readWutfacts;assert(widenf(nowut))),
     (member(wtoken,Options),assert(widenf(wtoken));assert(widenf(notoken))),
     (member(widenP(WPoints),Options);WPoints='widenpoints'),
     (member(narrowO(NOutput),Options);NOutput='stdnarrowout'),
     (member(xmlO(XOutput),Options);XOutput='result.xml'),
     (member(factFile(FactFile),Options);FactFile='chaFacts'),
     (member(narrowiterationsO(Nit),Options),atom_number(Nit,NitN);NitN is 0),
     (member(delaywiden(DWit),Options),atom_number(DWit,DWitN);DWitN is 0),
     assert(delays(DWitN)),
     assert(wdelayfor(DWitN)),

%Generate file with wideningpoints
%Note: command line option "-nowpscalc" should disable this
     detectwps(WPSMethod),
     (member(nowpscalc,Options);wto_file(File,WPSMethod,WPoints)),
     assert(operatorcount(0)),
     assert(outputfile(WOutput)),
     start_time,
     extract_program(File,IType),
     load_widenpoints(WPoints),
     readWutfacts,
     (flag(verbose)->true,write('Widening points: '),nl,showallwideningpoints;true),
     ppl_initialize,

     %Check version if appl.

     ppl_version(Pv),

     write('PPL version used: '),write(Pv),nl,

     %Flag everything for recompute in first iteration
     assert(flag(first)),
     !,

     %If "widen up to" is used, calculate convex hull for predicates

     widenupto,
     !,
     iterate,

     %Narrowing

     !,
     storeprenarrowfacts,

     %Transfer oldfacts to wfacts (widened facts).

     storewidenfacts,

     %Clear old and new facts

     assert(outputfile(NOutput)),
     assert(flag(first)),
     doNarrow(NitN),
     filewfacts,

     %For the webinterface - signal successful analysis

     write('Analysis Succeeded'),nl,
     end_time(user_output),
     %showwfacts,
     !,
     xmlResult(XOutput),
     factFile(FactFile),
     
     % make versions and build an automaton

     buildversions2,
     open('versions.out',write,VersionOutStream),
     showversions(VersionOutStream),
     close(VersionOutStream),
     assert(ftaCount(0)),
     versioniterate,
     %showversionFTA(user_output),
     %nl(user_output),
     %showversionpathFTA(user_output),
     %nl(user_output),
     %showatomicpropositions(user_output),
     %nl(user_output),
     write(user_output, 'Number of clauses in source program: '),
     clauseCount(K),
     write(user_output,K),
     nl(user_output),
     %write(user_output, 'Size of the path automaton: '),
     %ftaCount(FTAK),
     %write(user_output,FTAK),
     %nl(user_output),
     open('traceterm.out',write,S),
     findCounterexampleTrace(S),
     close(S),
     ppl_finalize.

cleanWorkspace :-
     retractall(flag(_)),
     retractall(currentflag(_)),
     retractall(nextflag(_)),
     retractall(operatorcount(_)),
     retractall(widening_point(_,_,_)),
     retractall(outputfile(_)),
     retractall(newfact(_,_)),
     retractall(oldfact(_,_)),
     retractall(wfact(_,_)),
     retractall(wutInitFact(_,_)),
     retractall(wutfact(_,_)),
     retractall(pnfact(_,_)),
     retractall(prio_widen(_)),
     retractall(widenAt(_)),
     retractall(widenf(_)),
     retractall(thistoken(_)),
     retractall(stripqueries(_)),
     retractall(supquery(_)),
     retractall(detectwps(_)),
     retractall(delays(_)),
     retractall(wdelayfor(_)),
     retractall(versionCount(_)),
     retractall(version(_,_,_)),
     retractall(clauseCount(_)),
     retractall(ftaCount(_)),
     retractall(pathtransition(_)),
     retractall(atomicproposition(_)),
     retractall(versiontransition(_,_)).

%%%%%%%%
%
%  Widen up to - prepare constraints
%
%
%%%%%%%%%%%

widenupto :-
     wutiterate,
     (flag(verbose) -> true, write('Widen up to facts '),nl,showwutfacts;true).

movewut :-
     wutfact(C,H),
     retractall(wutInitFact(C,_)),
     assert(wutInitFact(C,H)),
     retract(wutfact(C,H)),
     fail;true.

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

%%%%%
%
% checkInvariants
%
%%%%%


checkInvariants :-
	invariant(P,C),
    numbervars(C,0,_),
    numbervars(P,0,_),
	ppl_new_NNC_Polyhedron_from_constraints(C,H0),
	wfact(P,H1),
	(ppl_Polyhedron_contains_Polyhedron(H0,H1),write('invariant('),write(P),write(') :- '),write(C),write(' is a valid invariant'),nl),
	fail;true.

%%%% Read program to be analysed

%QA transform?
extract_program(File,IType) :-
     stripqueries(no),
     !,
     load_file(File,IType).


doNarrow(0).
doNarrow(X):-
     doNarrowIt(X),
     retractall(flag(first)),
     retractall(wfact(_,_)),
     storewidenfacts,
     retractall(oldfact(_,_)),
     retractall(newfact(_,_)),
     assert(flag(first)),
     Y is X-1,
     doNarrow(Y).

doNarrowIt(X) :-
     nl,write('----------- Iterate ----------'),nl,nl,
     !,
     narrowIteration,
     write('Narrowed facts '),write(X),nl,
     (flag(verbose)->true,showfacts;true).


load_widenpoints(WPfile) :-
     open(WPfile,read,WP),
     assert_widenpoints(WP).

assert_widenpoints(WP) :-
     read(WP,W),
     W \== end_of_file,
     assertWP(W),
     assert_widenpoints(WP).
assert_widenpoints(_) :-
      %Make sorted list of all widening points - sorted by number of loop they cut
      %Reverse it, so highest degree is head element

     findall((Dgs,F,N),widening_point(F/N,Dgs,_Delays),Wps),
     reverse(Wps,RWps),
     (flag(verbose) -> true,write('Ordered by degree '),write(RWps),nl;true),
     assert(prio_widen(RWps)).
     
assertWP(widening_point(X,Y)) :-
	 !,
	 delays(D),
     assert(widening_point(X,Y,D)).
assertWP(widening_point(X)) :-
	 !,
	 delays(D),
     assert(widening_point(X,0,D)).
     

conc([],L,L).
conc([A|L1],L2,[A|L3]) :-
     conc(L1,L2,L3).

%%%%%%%%%%%%%%%%
%
% Iterate continues until fixpoint is reached
%
%%%%%%%%%%%%%%%%

iterate:-
	%operator,
	widenIteration,
	retract(operatorcount(X)),
	Y is X + 1,
	assert(operatorcount(Y)),
	write('-'),write(X),
	retractall(flag(first)),
	fail.
iterate:-
	operatorcount(_),
	(flag(verbose)->true,showfacts;true),
	widenfacts,
	reiterate,
	switch_flags,
	!,
	iterate.
iterate :-
	nl.

%If anything was flagged "changed" for next iteration, then reiterate
%No flags -> it's safe to abort iterate

reiterate :-
      (nextflag(_/_) ;
	  filefacts,
	  fail),
      !.

widenIteration :-
     my_clause(H,B),
     operator(H,B),
     fail;true.

narrowIteration :- 
     my_clause(H,B),
     narrowOperator(H,B),
     fail.
narrowIteration.

% "Widen up to" iteration

wutInitIterate :-
     my_clause(H,B),
     wutInitOperator(H,B),
     fail.
wutInitIterate.

wutiterate :-
     my_clause(H,B),
     wutOperator(H,B),
     fail.
wutiterate.


%%%%%%%%%%%%%%%%
%
% "Operator" proves bodies/projects onto heads etc.
%
%%%%%%%%%%%%%%%%

operator(Head,B):-
	(changed(B);flag(first)),
	prove(B,Cs),
	varset((Head,Cs),Ys),
	Head =.. [_|Xs],
	linearize(Cs,CsLinNOP),

    %Make sure all variables occurs atleast once when naming anonymous vars.
    %Find smarter way of doing this

	dummyCList(Xs,DCLx),
	dummyCList(Ys,DCLy),
	append(DCLx,DCLy,DCL),
    append(CsLinNOP,DCL,CsLin),
	numbervars((Head:-CsLin),0,_),
	satisfiable(CsLin,H1),
	setdiff(Ys,Xs,Zs),
	project(H1,Zs,Hp),
	record(Head,Hp).


dummyCList([],[]).
dummyCList([C|Cs],[C-C=0|Cs1]) :-
        dummyCList(Cs,Cs1).

changed([]) :- fail.
changed([B|_]) :-
    isflagset(B).
changed([B|Bs]):-
    \+ isflagset(B),
    changed(Bs).

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
    Head =.. [_|Xs],
	narrowProve(B,Xs,Cs),
	varset((Head,Cs),Ys),
	linearize(Cs,CsLinNOP),
	dummyCList(Xs,DCLx),
	dummyCList(Ys,DCLy),
	append(DCLx,DCLy,DCL),
    append(CsLinNOP,DCL,CsLin),
	numbervars((Head:-CsLin),0,_),
	satisfiable(CsLin,H1),
	setdiff(Ys,Xs,Zs),
    !,
	project(H1,Zs,Hp),
	wfact_record(Head,Hp),
    !,
    true.

narrowProve([],_,[]).
narrowProve([true],_,[]).
narrowProve([B|Bs],Hv,[C|Cs]):-
	constraint(B,C),
	!,
	narrowProve(Bs,Hv,Cs).
narrowProve([B|Bs],Hv,Cs):-
	getwfact(B,Cs1),
	narrowProve(Bs,Hv,Cs2),
	append(Cs1,Cs2,Cs).

wutInitOperator(Head,B):-
    Head =.. [_|Xs],
	wutInitProve(B,Cs),
	varset((Head,Cs),Ys),
	linearize(Cs,CsLinNOP),
	dummyCList(Xs,DCLx),
	dummyCList(Ys,DCLy),
	append(DCLx,DCLy,DCL),
    append(CsLinNOP,DCL,CsLin),
	numbervars((Head:-CsLin),0,_),
	satisfiable(CsLin,H1),
	setdiff(Ys,Xs,Zs),
    !,
	project(H1,Zs,Hp),
	wutInitFact_record(Head,Hp),
    !,
    true.


wutInitProve([],[]).
wutInitProve([true],[]).
wutInitProve([B|Bs],[C|Cs]):-
	constraint(B,C),
	!,
	wutInitProve(Bs,Cs).
wutInitProve([_|Bs],Cs):-
	wutInitProve(Bs,Cs).

wutOperator(Head,B):-
    Head =.. [_|Xs],
	wutProve(B,Cs),
	varset((Head,Cs),Ys),
	linearize(Cs,CsLinNOP),
	dummyCList(Xs,DCLx),
	dummyCList(Ys,DCLy),
	append(DCLx,DCLy,DCL),
    append(CsLinNOP,DCL,CsLin),
	numbervars((Head:-CsLin),0,_),
	satisfiable(CsLin,H1),
	setdiff(Ys,Xs,Zs),
    !,
	project(H1,Zs,Hp),
	wutfact_record(Head,Hp),
    !,
    true.

wutProve([],[]).
wutProve([true],[]).
wutProve([B|Bs],[C|Cs]):-
	constraint(B,C),
	!,
	wutProve(Bs,Cs).
wutProve([B|Bs],Cs):-
    %Wut facts exists, propagate these

	wutInitFact(B,Handle),
	\+ ppl_Polyhedron_is_universe(Handle),
	!,
    ppl_Polyhedron_get_minimized_constraints(Handle,Cs1),
	wutProve(Bs,Cs2),
	append(Cs1,Cs2,Cs).
wutProve([_|Bs],Cs):-
	wutProve(Bs,Cs).

%%%%%%%%%%%%%%%%%
%
% Flagging new/old facts
%
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
        true
	;  
        assert(nextflag(Fn/N))
	).

wfact_record(F,H) :- 
     (flag(first) -> true,wfact_cond_assert(F,H));
     (write('Asserting nothing in second iteration'),nl,
	 sifact_cond_assert(F,H)).

wutInitFact_record(F,H) :- 
	getExistingWutInitConstraints(F,H0),
	convhull(H0,H,H2),
	retractall(wutInitFact(F,_)),
	assert(wutInitFact(F,H2)).

wutfact_record(F,H) :- 
	getExistingWutConstraints(F,H0),
	convhull(H0,H,H2),
	retractall(wutfact(F,_)),
	assert(wutfact(F,H2)).


record(F,H):-cond_assert(F,H).


cond_assert(F,H):-
	\+ (fact(F,H1), entails(H1,H)),
	getExistingConstraints(F,H0),
	convhull(H0,H,H2),
	assert(newfact(F,H2)),
	check_raise_flag(F,H0,H2).


wfact_cond_assert(F,H):-
	getExistingWConstraints(F,H0),
	convhull(H0,H,H2),
	retractall(oldfact(F,_)),
	assert(oldfact(F,H2)),
	check_raise_flag(F,H0,H2).


sifact_cond_assert(F,H):-
	getExistingWConstraints(F,H0),
	convhull(H0,H,_H2),
	retractall(oldfact(F,_)),
	assert(oldfact(F,H)).

getExistingConstraints(F,H0) :-
	retract(newfact(F,H0)),
	!.
getExistingConstraints(F,H0) :-
	oldfact(F,H0),
	!.
getExistingConstraints(_,empty).


getExistingWConstraints(F,H0) :-
	oldfact(F,H0),
	!.
getExistingWConstraints(_,empty).


getExistingWutInitConstraints(F,H0) :-
	wutInitFact(F,H0),
	!.
getExistingWutInitConstraints(_,empty).

getExistingWutConstraints(F,H0) :-
	wutfact(F,H0),
	!.
getExistingWutConstraints(_,empty).


check_raise_flag(F,empty,_) :-
	!,
	raise_flag(F).
check_raise_flag(_,H0,H2) :-
	equivalent(H0,H2),
	!.
check_raise_flag(F,_,_) :-
	raise_flag(F).



%%%%%%%%%%%%%%%%%
%
% Preds. handling/showing/filing facts
%
%%%%%%%%%%%%%%%%%

storeprenarrowfacts :-
    oldfact(H,Cs),
	assert(pnfact(H,Cs)),
	fail.
storeprenarrowfacts.

storewidenfacts :-
    oldfact(H,Cs),
	assert(wfact(H,Cs)),
	fail.
storewidenfacts.

getoldfact(B,Cs1) :-
    functor(B,F,N),
	functor(B1,F,N),
	oldfact(B1,H),
	ppl_Polyhedron_get_minimized_constraints(H,Cs2),
	melt((B1,Cs2),(B,Cs1)).

getoldfactslow(B,Cs1) :-
	oldfact(B1,H),
	ppl_Polyhedron_get_constraints(H,Cs2),	
	melt((B1,Cs2),(B,Cs1)).

getwfact(B,Cs1) :-
        \+ (flag(first)-> true),
        write('In getwfact'),nl,
        !,
        functor(B,F,N),
	    functor(B1,F,N),
        oldfact(B1,H),
        ppl_Polyhedron_get_constraints(H,Cs2),
        write('Trying '),write(Cs2),nl,
        melt((B1,Cs2),(B,Cs1)).
getwfact(B,Cs1) :-
    !,
    functor(B,F,N),
	functor(B1,F,N),
	wfact(B1,H),
	ppl_Polyhedron_get_constraints(H,Cs2),
	melt((B1,Cs2),(B,Cs1)).


getwfactslow(B,Cs1) :-
        \+ (flag(first)-> true),
        !,
        oldfact(B1,H),
        ppl_Polyhedron_get_constraints(H,Cs2),
        melt((B1,Cs2),(B,Cs1)).
getwfactslow(B,Cs1) :-
	wfact(B1,H),
	ppl_Polyhedron_get_constraints(H,Cs2),
	melt((B1,Cs2),(B,Cs1)).

fact(X,Y) :-
	newfact(X,Y).
fact(X,Y) :-
	oldfact(X,Y).
	
showallwideningpoints:-
     widening_point(X,Degree,Delays),
     write('  '),write(X),write(' Included in '),write(Degree),write(' program loops'),write(' - should be delayed for iterations = '),write(Delays),nl,
     fail.
showallwideningpoints.

showallfacts :-
	fact(F,C),
	numbervars(F,0,_),
	write(F), write(' :- '), write(C),nl,
	fail;true.
	
showwfacts :-
	wfact(F,H),
	ppl_Polyhedron_get_minimized_constraints(H,C),
	numbervars(F,0,_),
	write(F), write(' :- '), write(C),nl,
	fail;true.

showfacts :-
	oldfact(F,H),
	%pnfact(F,H),
	ppl_Polyhedron_get_minimized_constraints(H,C),
	numbervars(F,0,_),
	write(F), write(' :- '), write(C),nl,
	fail;true.
	
showoldfacts :-
	oldfact(F,H),
	ppl_Polyhedron_get_minimized_constraints(H,C),
	numbervars(F,0,_),
	write(F), write(' :- '), write(C),nl,
	fail;true.

showwutInitFacts :-
	wutInitFact(F,H),
	ppl_Polyhedron_get_minimized_constraints(H,C),
	numbervars(F,0,_),
	write('  '),write(F), write(' :- '), write(C),nl,
	fail;true.

showwutfacts :-
	wutfact(F,H),
	ppl_Polyhedron_get_minimized_constraints(H,C),
	numbervars(F,0,_),
	write('  '),write(F), write(' :- '), write(C),nl,
	fail;true.


showmyclauses :-
        my_clause(H,B),
        write(H),write(' :- '),write(B),nl,
        fail;true.

showinvariants :-
        invariant(H,B),
        write(H),write(' :- '),write(B),nl,
        fail;true.

filefacts :-
        outputfile(Outfile),
        open(Outfile,write,Sout),
        (oldfact(F,H),
        ppl_Polyhedron_get_minimized_constraints(H,C),
        numbervars(F,0,_),
        writeq(Sout,F), write(Sout,' :- '), writeq(Sout,C),nl(Sout),
        fail;
        close(Sout)).

filewfacts :-
        outputfile(Outfile),
        open(Outfile,write,Sout),
        (wfact(F,H),
        ppl_Polyhedron_get_minimized_constraints(H,C),
        numbervars(F,0,_),
        writeq(Sout,F), write(Sout,' :- '), writeq(Sout,C),nl(Sout),
        fail;
        close(Sout)).
        
factFile(File) :-
        open(File,write,Sout),
        (wfact(F,H),
        ppl_Polyhedron_get_minimized_constraints(H,C),
        numbervars(F,0,_),
        writeq(Sout,F), write(Sout,' :- '), 
        writeq(Sout,C),
        write(Sout,'.'),
        nl(Sout),
        fail;
        close(Sout)).

%%%%%%%%%%%%%%
%
% Convex Hull Operations
%
%%%%%%%%%%%%%%

satisfiable(Cs,H1) :-
	ppl_new_NNC_Polyhedron_from_constraints(Cs,H1),
	\+ ppl_Polyhedron_is_empty(H1).

project(H,Zs,H) :-

    %For debugging - what is being projected away?

	ppl_Polyhedron_remove_space_dimensions(H,Zs).


convhull(empty,H1,H1) :-
	!.
convhull(H1,empty,H1) :-
	!.

convhull(H0,H1,H2) :-
	ppl_Polyhedron_poly_hull_assign(H1,H0),
	H2 = H1,
    true.

entails(H0,H1) :-
	checkEntails(H0,H1).
	
checkEntails(H0,_) :-
	ppl_Polyhedron_is_universe(H0),
	!.
checkEntails(H0,H1) :-
	\+ ppl_Polyhedron_is_universe(H1),
	ppl_Polyhedron_contains_Polyhedron(H0,H1).
	
equivalent(H0,H1) :-
	ppl_Polyhedron_equals_Polyhedron(H0,H1).




%%%%%%%%%%%%%
%
% Widening
%
%%%%%%%%%%%%%

%Widenfacts must fail, with the exception of the last cls.
%This version is for candidates chosen from Bourdoncle's method

widenfacts_old :-
     delays(X),
     X > 0,
     Y is X - 1,
     retractall(delays(_)),
     assert(delays(Y)),
     write('Skipping a wideniteration'),nl,
     newoldfacts,
     !.

widenfacts_old :-
     delays(0),
     gowidenfacts,
     fail.
widenfacts_old :-
     !,
     retractall(delays(_)),
     wdelayfor(X),
     assert(delays(X)),
     newoldfacts.

widenfacts :-
     gowidenfacts,
	newoldfacts.

gowidenfacts :-

    %From prio_widen-list, build list of predicates with both old and new facts.
    %Widen this list (possibly only head element).

     prio_widen(PrioWiden),
     possibleWidenPs(PrioWiden,PosPW),
     (flag(verbose)->true,write('Possible Wideningpoints '),write(PosPW),nl;true),
     !,
     widenlist(PosPW).


possibleWidenPs([],[]).
possibleWidenPs([(Dg,F,N)|WPs],[Fn|PWPs]) :-
      %showallwideningpoints,nl,

      widening_point(F/N,Dg,_Delays),
      functor(Fn,F,N),
      newfact(Fn,_),
      oldfact(Fn,_),
      !,
      possibleWidenPs(WPs,PWPs).
possibleWidenPs([(_,_,_)|WPs],PWPs) :-
      !,
      possibleWidenPs(WPs,PWPs).

goallwidenfacts :-
     widening_point(Fn/N,_Degree,_Delays),
     functor(F,Fn,N),
     newfact(F,_),
     oldfact(F,_),
     widenlist([F]),!.

newoldfacts :-
     retract(newfact(F,H)),
     retractall(oldfact(F,_)),
     assert(oldfact(F,H)),
     fail.
newoldfacts.


widenlist([]).
widenlist([Wc|Ws]) :-
    %If widening with tokens, do not decrement

    widenf(wtoken),
	functor(Wc,WcF,WcN),
    widening_point(WcF/WcN,_,K),
	!,
    retractall(thistoken(_)),
    assert(thistoken(K)),
    retract(newfact(Wc,NewH)),
    retract(oldfact(Wc,OldH)),
    (flag(verbose)->true, write('Widening at '),write(Wc),nl;true),
    !,
    wutwiden(Wc,NewH,OldH,H2),
    thistoken(K2),
    retractall(widening_point(WcF/WcN,TX,_)),
    assert(widening_point(WcF/WcN,TX,K2)),
    (widenAt(singlepoint),assert(oldfact(Wc,H2));
    widenAt(allpoints),assert(oldfact(Wc,H2)),
    widenlist(Ws)).
widenlist([Wc|Ws]) :-
	functor(Wc,WcF,WcN),
	widening_point(WcF/WcN,P,D),
	D > 0,
	!,
	ND is D - 1,
	retractall(widening_point(WcF/WcN,P,D)),
	assert(widening_point(WcF/WcN,P,ND)),
	widenlist(Ws).
widenlist([Wc|Ws]) :-
	functor(Wc,WcF,WcN),
    widening_point(WcF/WcN,_,0),
	!,
    retract(newfact(Wc,NewH)),
    retract(oldfact(Wc,OldH)),
    (flag(verbose)->true, write('Widening at '),write(Wc),nl;true),
    !,
    wutwiden(Wc,NewH,OldH,H2),
    (widenAt(singlepoint),assert(oldfact(Wc,H2))
    ;
    widenAt(allpoints),
    assert(oldfact(Wc,H2)),
	widenlist(Ws)).


boundbelow([],[]).
boundbelow([X|Xs],[X>=0|Ys]) :-
        boundbelow(Xs,Ys).

	
widen2(F,H0,H1,H2) :-
    ppl_Polyhedron_space_dimension(H0,Dims),
	length(VL,Dims),
	boundbelow(VL,VLb),
	numbervars(VLb,0,_),
	write('Bounding below with '),write(VLb),nl,
	ppl_Polyhedron_limited_H79_extrapolation_assign(H0,H1,VLb),
	H2 = H0,
    (
	    %If equivalent - don't flag
       (equivalent(H1,H2),!,

         %Debugging - which are considered equivalent

            write('Equivalent cnstr. after widening'),nl,
            ppl_Polyhedron_get_constraints(H1,Cls1),
            ppl_Polyhedron_get_constraints(H2,Cls2),
            write(Cls1),nl,
            write(Cls2),nl
	    )
	    ;
	    %if not equivalent - flag for recompute

	    (raise_flag(F),!,
	     write('Widening of '),write(F),write(' changed constraints'),nl
	    )
	).

	
widen(F,H0,H1,H2) :-

    %%Debug only - follow results of widening

	(widenf(h79),ppl_Polyhedron_H79_widening_assign(H0,H1)
    ;
	    widenf(bhrz03),ppl_Polyhedron_BHRZ03_widening_assign(H0,H1)),
	    H2 = H0,
        (
	    %If equivalent - don't flag

	      (equivalent(H1,H2),!)
	    ;
	      %if not equivalent - flag for recompute

	      (raise_flag(F),!

	    )
	).

wutwiden(F,H0,H1,H2) :-

    %%Debug only - follow results of widening

    widenWRToptions(F,H0,H1),

	H2 = H0,
        (
	    %If equivalent - don't flag

	    (equivalent(H1,H2),!)
	    ;
	    %if not equivalent - flag for recompute

	    (raise_flag(F),!

	    )
	).


widenWRToptions(_,H0,H1) :-
     widenf(h79),
     widenf(nowut),
     widenf(notoken),
     !,
     ppl_Polyhedron_H79_widening_assign(H0,H1).
widenWRToptions(F,H0,H1) :-
     widenf(h79),
     widenf(withwut),
     widenf(bounded),
     widenf(notoken),
     !,
     wutfact(F,Hwut),
     ppl_Polyhedron_get_constraints(Hwut,Cap),
     addInvariants(F,Cap,Cns),
     (flag(verbose) -> write('Widen up2 Cns: '),write(Cns),nl; true),
     ppl_Polyhedron_bounded_H79_extrapolation_assign(H0,H1,Cns).


%These can be deleted (modelcheck)
widenWRToptions(F,H0,H1) :-
     widenf(h79),
     widenf(withwut),
     widenf(modelcheck),
     widenf(notoken),

     write('F : '),write(F),nl,

     invariant(F,Cns),
     !,
     ppl_Polyhedron_limited_H79_extrapolation_assign(H0,H1,Cns).
widenWRToptions(_,H0,H1) :-
     widenf(h79),
     widenf(withwut),
     widenf(modelcheck),
     widenf(notoken),
     !,
     ppl_Polyhedron_H79_widening_assign(H0,H1).
widenWRToptions(_,H0,H1) :-
     widenf(h79),
     widenf(withwut),
     widenf(limited),
     widenf(notoken),
     !,
     wutfact(F,Hwut),
     ppl_Polyhedron_get_constraints(Hwut,Cap),
     addInvariants(F,Cap,Cns),
     ppl_Polyhedron_limited_H79_extrapolation_assign(H0,H1,Cns).
widenWRToptions(_,H0,H1) :-
     widenf(bhrz03),
     widenf(nowut),
     widenf(notoken),
     !,
     ppl_Polyhedron_BHRZ03_widening_assign(H0,H1).
widenWRToptions(F,H0,H1) :-
     widenf(bhrz03),
     widenf(withwut),
     widenf(bounded),
     widenf(notoken),
     !,
     wutfact(F,Hwut),
     ppl_Polyhedron_get_minimized_constraints(Hwut,Cap),
     addInvariants(F,Cap,Cns),
     ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign(H0,H1,Cns).
widenWRToptions(F,H0,H1) :-
     widenf(bhrz03),
     widenf(withwut),
     widenf(limited),
     widenf(notoken),
     !,
     wutfact(F,Hwut),
     ppl_Polyhedron_get_constraints(Hwut,Cap),
     addInvariants(F,Cap,Cns),
     ppl_Polyhedron_limited_BHRZ03_extrapolation_assign(H0,H1,Cns).
widenWRToptions(_F,H0,H1) :-
     widenf(wtoken),
     widenf(h79),
     widenf(nowut),
     !,
     thistoken(K),
     ppl_Polyhedron_H79_widening_assign_with_tokens(H0,H1,K,K2),
     retractall(thistoken(_)),
     assert(thistoken(K2)).
widenWRToptions(_,H0,H1) :-
     widenf(wtoken),
     widenf(bhrz03),
     widenf(nowut),
     !,
     thistoken(K),
     ppl_Polyhedron_BHRZ03_widening_assign_with_tokens(H0,H1,K,K2),
     retractall(thistoken(_)),
     assert(thistoken(K2)).
widenWRToptions(F,H0,H1) :-
     widenf(wtoken),
     widenf(h79),
     widenf(withwut),
     widenf(bounded),
     !,
     wutfact(F,Hwut),
     thistoken(K),
     ppl_Polyhedron_get_minimized_constraints(Hwut,Cap),
     addInvariants(F,Cap,Cns),
     ppl_Polyhedron_bounded_H79_extrapolation_assign_with_tokens(H0,H1,Cns,K,K2),
     retractall(thistoken(_)),
     assert(thistoken(K2)).
widenWRToptions(F,H0,H1) :-
     widenf(wtoken),
     widenf(bhrz03),
     widenf(withwut),
     widenf(bounded),
     !,
     wutfact(F,Hwut),
     thistoken(K),
     ppl_Polyhedron_get_minimized_constraints(Hwut,Cap),
     addInvariants(F,Cap,Cns),
     ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_tokens(H0,H1,Cns,K,K2),
     retractall(thistoken(_)),
     assert(thistoken(K2)).
widenWRToptions(F,H0,H1) :-
     widenf(wtoken),
     widenf(h79),
     widenf(withwut),
     widenf(limited),
     !,
     wutfact(F,Hwut),
     thistoken(K),
     ppl_Polyhedron_get_minimized_constraints(Hwut,Cap),
     addInvariants(F,Cap,Cns),
     ppl_Polyhedron_limited_H79_extrapolation_assign_with_tokens(H0,H1,Cns,K,K2),
     retractall(thistoken(_)),
     assert(thistoken(K2)).
widenWRToptions(F,H0,H1) :-
     widenf(wtoken),
     widenf(bhrz03),
     widenf(withwut),
     widenf(limited),
     !,
     wutfact(F,Hwut),
     thistoken(K),
     ppl_Polyhedron_get_minimized_constraints(Hwut,Cap),
     addInvariants(F,Cap,Cns),
     ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_tokens(H0,H1,Cns,K,K2),
     retractall(thistoken(_)),
     assert(thistoken(K2)).


addInvariants(F,Cin,Cout) :-
     bagof(Cs,invariant(F,Cs),Clist),
     !,
     flattenList(Clist,Cf),
     append(Cf,Cin,Cout).
addInvariants(_,Cs,Cs).

flattenList([],[]).
flattenList([L|Ls],Lout) :-
     flattenList(Ls,Lpre),
     append(L,Lpre,Lout).

%%%%%%%%%%%%
%
% Simple Cousot narrowing (to 0)
%
%%%%%%%%%%%%

%foreach dimension intersect with dim>=0




%%%%%%%%%%%%
%
% Linearise
%
%%%%%%%%%%%%
	
constraint(X=Y, X=Y).
constraint(X=:=Y, X=Y).
constraint(X is Y, X = Y).
constraint(X>Y, X>Y).
constraint(X>=Y, X>=Y).
constraint(X=<Y, X=<Y).
constraint(X<Y, X<Y).

%How to handle diseq.?
constraint(_\==_,0=0).
constraint(_=\=_,0=0).
constraint(true,0=0).
constraint(fail,1=0).



linearize([],[]).
linearize([C|Cs],[C|Cs1]) :-
	linear_constraint(C),
	!,
	linearize(Cs,Cs1).

%Version with linear approx. goes here

linearize([C|Cs],[C1|Cs1]) :-
	linear_approx(C,C1),
	!,
	linearize(Cs,Cs1).
linearize([_C|Cs],Cs1) :-
		linearize(Cs,Cs1).
linear_constraint(X = Y) :-
	linear_term(X),
	linear_term(Y).
linear_constraint(X is Y) :-
	linear_term(X),
	linear_term(Y).
linear_constraint(X>Y) :-
	linear_term(X),
	linear_term(Y).
linear_constraint(X<Y) :-
	linear_term(X),
	linear_term(Y).
linear_constraint(X>=Y) :-
	linear_term(X),
	linear_term(Y).
linear_constraint(X=<Y) :-
	linear_term(X),
	linear_term(Y).

linear_term(X) :- 
	const(X),!.
linear_term(X) :-
	var(X),!.
linear_term(-(X)) :-
	linear_term(X).
linear_term(+(X)) :- 
	linear_term(X).
linear_term(X+Y) :- 
	linear_term(X),
	linear_term(Y).
linear_term(X-Y) :- 
	linear_term(X),
	linear_term(Y).
linear_term(X*Y) :- 
	const(X),
	linear_term(Y);
	const(Y),
	linear_term(X).
linear_term(X/Y) :- 
	const(Y),
	linear_term(X).

const(X) :-
	number(X),!.
const(X) :-
	var(X),!,fail.
const(X+Y) :-
	const(X),
	const(Y).
const(X-Y) :-
	const(X),
	const(Y).
const(X*Y) :-
	const(X),
	const(Y).
const(X/Y) :-
	const(X),
	const(Y).


linear_approx(X = Y \/ Z,X = Const) :-
	const(Y),const(Z),Const is Y \/ Z,!.
linear_approx(X = Y \/ Z, X =< Y + Z) :-
       const(X),!.


%Should be =< and not X < Y + Z
linear_approx(X = Y \/ Z, X =< Y + Z) :- !.
linear_approx(X = Y /\ Z,X = Const) :-
	const(Y),const(Z),Const is Y /\ Z,!.
linear_approx(X = Y /\ Z,X =< Y + Z) :-
	const(X),!.
linear_approx(X = Y /\ Z, X =< Y + Z) :- !.

%
% XOR
%

linear_approx(X = Y # Z,X = Const) :-
        const(Y),const(Z),!, Const is Y # Z.

%Do not remove variables!

linear_approx(X = Y # Z, X < 256 + Y-Y) :- const(Z),!.
linear_approx(X = Y # Z, X < 256 + Y+Z) :- !.

%Shift right

linear_approx(X = Y>>Z,X = W) :-
 	const(Y),const(Z),
 	W is Y>>Z,!.
linear_approx(X = Y>>_,X=0) :- 
	const(Y),Y is 0, !.
linear_approx(X = _>>Z,X=0) :-
	const(Z), Z >= 8, !.
linear_approx(X = Y>>Z,Y<256) :- 
	const(X), X is 0, const(Z), Z >= 8,!.
linear_approx(X = Y>>Z,X>=Y) :-
	const(Z),!.

%Introducing an extra variable (T1) is a problem...
linear_approx(X = Y << Z,X=Y*T1) :-
	const(Z), !,
	T1 is truncate(2 ** Z).
linear_approx(X = Y ** Z,X=T1) :-
        const(Y),
	const(Z), !,
	T1 is truncate(Y ** Z).


%At the moment, it is not needed to separate const. Y from var. Y

linear_approx(X = \Y ,X=255-Y) :-
	var(Y), !.
linear_approx(X = \Y, X = 255 - Y) :-
	const(Y), !.

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
         Options = OT,     Args = [X|AT],
         RT = T
       )
   ),
   get_options(RT,OT,AT).

recognised_option('-prg',programO(R),[R]).
recognised_option('-pic',pic,[]).
recognised_option('-widenpoints',widenP(R),[R]).
recognised_option('-widenout',widenO(R),[R]).
recognised_option('-narrowout',narrowO(R),[R]).
recognised_option('-narrowiterations',narrowiterationsO(R),[R]).
recognised_option('-delaywidening',delaywiden(R),[R]).
recognised_option('-xml',xmlO(R),[R]).
recognised_option('-wfunc',widenF(F),[F]).
recognised_option('-v',verbose,[]).
recognised_option('-querymodel',querymodel(Q),[Q]).
recognised_option('-nowpscalc',nowpscalc,[]).
recognised_option('-withwut',withwut(T),[T]).
recognised_option('-wtoken',wtoken,[]).
recognised_option('-singlepoint',singlepoint,[]).
recognised_option('-detectwps',detectwps(M),[M]).
recognised_option('-o',factFile(F),[F]).

%%%%%%%%%%%
%
% XML output
%
%%%%%%%%%%%

xmlResult(OF) :-
     open(OF,write,OS),
     setof(X,genclsx(X),Xs),
     xmlHeader(OS),
     write(OS,'<bucha>'),nl(OS),
     xmlOutput(Xs,OS,0),
     write(OS,'</bucha>'),nl(OS),
     close(OS).

queryClean :-
    %if command line option querymodel, remove _query from facts

    stripqueries(yes),
    qClean,!.
queryClean.
qClean :-
       my_clause(H,B),

       %If it is a _query pred., dispose of it

       H =.. [F|_],
       name(F,Fna),
       append(_,[95,113,117,101,114,121],Fna),
       write('F removed '),write(F),nl,
       retractall(my_clause(H,B)),
       fail.
qClean.

genclsx(clause(head(string(H),widen(string(Wcs)),narrow(string(Ncs))),body(BA))) :-
     my_clause(H,B),
     numbervars((H :- B),0,_),
     genbodyatoms(B,BA),
     getcns(H,Wcs,Ncs).

genbodyatoms([],[]).
genbodyatoms([B|Bs],[bodyAtom(string(B))|BAs]) :-
     genbodyatoms(Bs,BAs).

getcns(H,Wcs,Ncs) :-
     (pnfact(H,Hw),ppl_Polyhedron_get_constraints(Hw,Wcs);Wcs=[empty]),
     (wfact(H,Hn),ppl_Polyhedron_get_constraints(Hn,Ncs);Ncs=[empty]),
     !.

myclsshow :-
     my_clause(H,B),
     write(H),write(' :- '),write(B),nl,
     fail.
myclsshow.


% Version generation and FTA construction

fact3(F,H,_) :-
	wfact(F,H).

buildversions2 :-
	assert(versionCount(0)),
	fact3(F,H,_),
	%functor(F,P,_),
	%name(P,PName),
	%append("rState",_,PName), 	% only select the transition predicates
	retract(versionCount(N1)),
	N is N1+1,
	assert(versionCount(N)),
	assert(version(F,H,N)),
	fail.
buildversions2.

showversions(S) :-
	version(F,H,K),
	getConstraint(H,Cs0),
	write(S,K), write(S,': '), 
	write(S,F), write(S,' :- '), write(S,Cs0),
	nl(S),
	fail.
showversions(_).

writeversionbody([],S) :-
	!,
	write(S,'.').
writeversionbody([B],S) :-
	!,
	write(S,B),
	write(S,'.').
writeversionbody([B|Bs],S) :-
	write(S,B),
	writecomma(S),
	writeversionbody(Bs,S).
	
writecomma(S) :-
	write(S,',').


versioniterate :-
	assert(clauseCount(0)),
    versionoperator,
	fail.
versioniterate.

versionoperator :-
	my_clause(Head,B),
	retract(clauseCount(K)),
	K1 is K+1,
	assert(clauseCount(K1)),
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
	%assert(versionclause(Hv,Vs)),
	assertTransition(Hv,Vs,K1).

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
	assert(versiontransition(Head,Body)),
	makeHpath(Body,Head,HPath),
	makeBpath(Body,_BPath),
	makeAtomicPropositionFact(Body,Head,Prop),
	assert(pathtransition(HPath)),
	assert(atomicproposition(Prop)).


%%%%%%%%%%%%%%%%%% Edited by GNB *begins* %%%%%%%%%%%%%%%%%%

makeAtomicPropositionFact(true,Head,Prop) :-
	!,
	Head =.. [R|_],
	Prop =.. [prop,R,R].
makeAtomicPropositionFact(Body,_Head,Prop) :-
	Body =.. [R1,_X],
	Prop =.. [prop,R1,R1].

%%%%%%%%%%%%%%%%%% Edited by GNB *ends* %%%%%%%%%%%%%%%%%%%%

makeBpath(true, true) :-
	!.
makeBpath(Body,BPath) :-
	Body =.. [R1,X],
	BPath =.. [path,[R1|X]].

%%%%%%%%%%%%%%%%%% Edited by GNB *ends* %%%%%%%%%%%%%%%%%%%%

 makeHpath(true,Head,HPath) :-
        !,
        Head =.. [R|_],
        HPath =.. [initState,R].
 makeHpath(Body,Head,HPath) :-
	Body =.. [R1,_X],
	Head =.. [R,_],
	HPath =.. [trans,R1,R].

%%%%%%%%%%%%%%%%%% Edited by GNB *ends* %%%%%%%%%%%%%%%%%%%%

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
	
stateSymb(H,R) :-
	functor(H,F,_),
	name(F,T),
	append(_,[95|Xs],T),
	\+ member(95,Xs),
	name(R,Xs).
	
clauseFunctor(N1,F) :-
	name(N1,M),
	append("c",M,CM),
	name(F,CM).

	
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
	
showversionFTA(S) :-
    write('writing version fta'), nl,
	versiontransition(H,B),
	retract(ftaCount(K)),
	K1 is K+1,
	assert(ftaCount(K1)),
	write(S,(H :- B)),
	write(S,'.'),
	nl(S),
	fail.
showversionFTA(_).

showversionpathFTA(S) :-
	bagof(Y,pathtransition(trans(X,Y)),Ys),
	H =.. [nextStates,X,Ys],
	write(S,H),
	write(S,'.'),
	nl(S),
	fail.
showversionpathFTA(S) :-
	bagof(Y,pathtransition(initState(Y)),Ys),
	H =.. [initStates,Ys],
	write(S,H),
	write(S,'.'),
	nl(S),
	fail.
showversionpathFTA(_).


showatomicpropositions(S) :-
	%atomicproposition(H),
	version(A,_,K),
	functor(A,F,_),
	name(F,FN),
	name(K,KN),
	append("v",KN,VKN),
	append(FN,[95|VKN],FNVK),
	name(_FVK,FNVK),
	name(VK,VKN),
	H =.. [prop,VK,VK],
	write(S,H),
	write(S,'.'),
	nl(S),
	fail.
showatomicpropositions(_).
 
showFunctionInvokedOnATransition4Uppaal(S) :-
	write(S,'</declaration><template><name x="5" y="5">AbstractTransitionSystem</name><declaration>'),
	nl(S),
	write(S,'void setAbstractStateId(int abStId)'),
	nl(S),
	write(S,'{'),
	nl(S),
	tab(S,8),
	write(S,'int i = 0;'),
	nl(S),
	tab(S,8),
	write(S,'while (i &lt;= N-1 )'),
	nl(S),
	write(S,'	{'),
	nl(S),
	tab(S,16),
	write(S,'if (i==abStId)'),
	nl(S),
	write(S,'			{	v[i] = 1;	}'),
	nl(S),
	write(S,'		else'),
	nl(S),
	write(S,'			{	v[i] = 0;	}'),
	nl(S),
	write(S,'		i++;'),
	nl(S),
	write(S,'	}'),
	nl(S),
	write(S,'}'),
	nl(S),
	write(S,'</declaration>'),
	nl(S).

	
showSystemDeclaration4Uppaal(S) :-
	write(S,'</template>'),
	nl(S),
	write(S,'<system>system AbstractTransitionSystem;</system>'),
	nl(S),
	write(S,'</nta>').


%%<?xml version="1.0" encoding="utf-8"?><!DOCTYPE nta PUBLIC '-//Uppaal Team//DTD Flat System 1.1//EN' %%\'http://www.it.uu.se/research/group/darts/uppaal/flat-1_1.dtd\'><nta><declaration>

showHeader4Uppaal(S) :-
	write(S,'<?xml version="1.0" encoding="utf-8"?><!DOCTYPE nta PUBLIC \'-//Uppaal Team//DTD Flat System 1.1//EN\' \'http://www.it.uu.se/research/group/darts/uppaal/flat-1_1.dtd\'><nta><declaration>'),
	nl(S).

	
showNumberOfLocations4Uppaal(S) :-
	versionCount(NumbOfLocations),
	atom_number(NumbOfLocationsAtom,NumbOfLocations),
	atom_concat('const int N=',NumbOfLocationsAtom,LocationNumberDecl1),
	atom_concat(LocationNumberDecl1,';',LocationNumberDecl),
	write(S,LocationNumberDecl),
	nl(S),
	makeLocationInitialisationArray(NumbOfLocations,LocationInitialisationArray),
	atom_concat('bool v[N]=',LocationInitialisationArray,LocationInitialisation1),
	atom_concat(LocationInitialisation1,';',LocationInitialisation),
	write(S,LocationInitialisation),
	nl(S).

makeLocationInitialisationArray(NumbOfLocations,LocationInitialisationArray) :-
	bagof(Y,pathtransition(initState(Y)),[InitVersionId|_]),
	atom_concat(v,InitIdAtom,InitVersionId),
	atom_number(InitIdAtom,InitId),
	generateBracedList(NumbOfLocations,InitId,0,'{',LocationInitialisationArray).
	
generateBracedList(NumbOfLocations,Init,Index,Prefix,LocationInitialisationArray) :-
	Index < NumbOfLocations,
	Index1 is Index+1,
	(
		(
			Index1 \= Init, 
			(
				(
				Index1 \= NumbOfLocations, 
				atom_concat(Prefix,'0,',Prefix1)
				)
			;
				(
				Index1 = NumbOfLocations, 
				atom_concat(Prefix,'0',Prefix1)
				)
			)	
		)
	;
	 	(
			Index1 = Init,
			(
				(
				Index1 \= NumbOfLocations, 
				atom_concat(Prefix,'1,',Prefix1) 
				)
			;
	 			(
				Index1 = NumbOfLocations,
				atom_concat(Prefix,'1',Prefix1)
				)
			)
		)
	),	
	generateBracedList(NumbOfLocations,Init,Index1,Prefix1,LocationInitialisationArray).

generateBracedList(NumbOfLocations,_Init,Index,Prefix,LocationInitialisationArray) :-
	Index = NumbOfLocations,
	atom_concat(Prefix,'}',LocationInitialisationArray).	
	
	
showtransitions4Uppaal(S) :-
	bagof(Y,pathtransition(trans(X,Y)),Ys),
	weaveTransitions4Uppaal(S,X,Ys),
	nl(S),
	fail.

showtransitions4Uppaal(_).

% <transition><source ref="id0"/><target ref="id0"/><label kind="assignment" >setAbstractStateId(2)</label></transition>

weaveTransitions4Uppaal(_S,_X,[]).
weaveTransitions4Uppaal(S,X,[Y|Ys]) :-
	atom_concat('<transition><source ref="',X,SourcePart),
	atom_concat(SourcePart,'"/><target ref="',SourceDestPartial),
	atom_concat(SourceDestPartial,Y,SourceDest),
	atom_concat(SourceDest,'"/><label kind="assignment" >setAbstractStateId(',SourceDest2),
	atom_length(Y,DestLocationIdLength),
	OneLessLengthOfDestLocationId is DestLocationIdLength - 1,
	sub_atom(Y,1,OneLessLengthOfDestLocationId,DestLocationNumericIdAsAtom),
	atom_number(DestLocationNumericIdAsAtom,DestLocationNumericIdAsNumber),
	OneLessDestLocationNumericId is DestLocationNumericIdAsNumber - 1,
	atom_number(OneLessDestLocationNumericIdAsAtom,OneLessDestLocationNumericId),
	atom_concat(SourceDest2,OneLessDestLocationNumericIdAsAtom,SourceDest3),
	atom_concat(SourceDest3,')</label></transition>',TransitionString),
	write(S,TransitionString),
	nl(S),
	weaveTransitions4Uppaal(S,X,Ys).


weaveTransitions4UppaalBackUp(S,X,[Y]) :-
	atom_concat('<transition><source ref="',X,SourcePart),
	atom_concat(SourcePart,'"/><target ref="',SourceDestPartial),
	atom_concat(SourceDestPartial,Y,SourceDest),
	atom_concat(SourceDest,'"/><label kind="assignment" >setAbstractStateId(',SourceDest2),
	atom_length(Y,DestLocationIdLength),
	OneLessLengthOfDestLocationId is DestLocationIdLength - 1,
	sub_atom(Y,1,OneLessLengthOfDestLocationId,DestLocationNumericIdAsAtom),
	atom_number(DestLocationNumericIdAsAtom,DestLocationNumericIdAsNumber),
	OneLessDestLocationNumericId is DestLocationNumericIdAsNumber - 1,
	atom_number(OneLessDestLocationNumericIdAsAtom,OneLessDestLocationNumericId),
	atom_concat(SourceDest2,OneLessDestLocationNumericIdAsAtom,SourceDest3),
	atom_concat(SourceDest3,')</label></transition>',TransitionString),
	write(S,TransitionString),
	nl(S).
		

showinitialstates4Uppaal(S) :-
	bagof(Y,pathtransition(initState(Y)),Ys),
	weaveInitialStates4Uppaal(S,Ys),%	H =.. [initStates,Ys],	write(S,H),	write(S,'.')
	nl(S),
	fail.

showinitialstates4Uppaal(_).


weaveInitialStates4UppaalBackUp(S,[Y]) :-
	atom_concat('<init ref="',Y,Init),
	atom_concat(Init,'"/>',InitialState),
	write(S,InitialState),
	nl(S).
weaveInitialStates4Uppaal(_S,[]).
weaveInitialStates4Uppaal(S,[Y|Ys]) :-%	H =.. [initStates,Ys],	write(S,H),	write(S,'.')
	atom_concat('<init ref="',Y,Init),
	atom_concat(Init,'"/>',InitialState),
	write(S,InitialState),
	nl(S),
	weaveInitialStates4Uppaal(S,Ys).


showlocations4Uppaal(S) :-
	version(A,_,K),
	functor(A,F,_),
	name(F,FN),
	name(K,KN),
	append("v",KN,VKN),
	append(FN,[95|VKN],FNVK),
	name(_FVK,FNVK),
	name(VK,VKN),

	%<location id="id0"><name>v3</name><urgent/></location>

	atom_concat('<location id="',VK,LocationPart1),
	atom_concat(LocationPart1,'"><name>',LocationPart2),
	atom_concat(LocationPart2,VK,LocationPart3),
	atom_concat(LocationPart3,'</name><urgent/></location>',Location),
	write(S,Location),
	nl(S),
	fail.
showlocations4Uppaal(_).

getConstraint(H,Cs0) :-
	ppl_Polyhedron_get_minimized_constraints(H,Cs0).
	
/*	
findCounterexampleTrace(S) :-
	version(false,_,Y),
	operatorcount(J),
	name(Y,K),
	append("v",K,VK),
	name(VN,VK),
	Goal =.. [VN,X],
	for(I,1,J),  % iterative deepening
	findTrace(Goal,I),
	!,
	write(S,counterexample(X)),
	write(S,'.'),
	nl(S),
	write(user_output,X),
	write(user_output,'.'),
	nl(user_output).
findCounterexampleTrace(S) :-
	write(S,'safe'),
	write(S,'.'),
	nl(S).	
*/

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
% :-	!.
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
	