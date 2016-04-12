% 
% Special attention: the output file is append (not write) which works
% only in this setting.
% 
% computes interpolant automata from a given set of Horn clauses and an
% error trace.
% 
% The error trace is given in the form counterexample(TraceTerm) e.g.
% 
% counterexample(c1(c2(c14(c16(c16(c16(c16(c17(c19(c23(c27(c31(c5(c7(c10(c12))))))))))))))))
% 
% Implementation Details:
% 
% 1. Tree interpolation algorithm is adapted from the paper by Régis
% Blanc, Ashutosh Gupta, Laura Kovács, Bernhard Kragl: Tree
% Interpolation in Vampire. LPAR 2013: 173-181
% 
% 2. Interpolant automata is based on the paper by: Weifeng Wang and Li
% Jiao: Trace Abstraction Refinement for Solving Horn Clauses Technical
% report Nr. ISCAS-SKLCS-15-19,State Key Laboratory of Computer Science,
% Institute of Software, Chinese Academy of Sciences.
% 

:- module(interpolantAutomaton, [main/1], []).

:- dynamic(interpolant/1).
:- dynamic(ftaTransition/1).

:- use_module(library(write)).
:- use_module(library(read)).
:- use_module(library(dynamic)).
:- use_module(library(aggregates)).

:- use_module(chclibs(interpolant)).
:- use_module(chclibs(linearize)).
:- use_module(chclibs(ppl_ops)).
:- use_module(chclibs(setops)).
:- use_module(chclibs(input_ppl_clausenum)).
:- use_module(chclibs(canonical)).
:- use_module(chclibs(yices2_sat)).
:- use_module(chclibs(common)).

:- use_module(library(terms_vars)).
:- use_module(library(ppl)).
:- use_module(library(lists)).
:- use_module(ciao_yices(ciao_yices_2)).

:- include(chclibs(get_options)).

recognised_option('-prg',  programO(R),[R]).
recognised_option('-trace', traces(R),[R]).
recognised_option('-o',generateAut(R),[R]).

go1:-
    ppl_initialize,
    %T= counterexample(c4(c1)),
    %T= counterexample(c4(c2(c1,c1))),
    T1=counterexample(c4(c3(c1, c2(c1)))),
    %yices_init,
    %computeInterpolantAutomaton('examples/mc91.pl',false,T),
    %computeInterpolantAutomaton('examples/fib.pl',false,T),
    computeInterpolantAutomaton('examples/trabs.pl',false,T1),
    %yices_exit,
    printFTA(T1, user_output),
    ppl_finalize.

go:-
    go2('Examples/running.nts.pl.pe.pl', 'Examples/trace.pl').

go2(F, T):-
    main(['-prg', F, '-trace', T]).

%error trace starts with the predicate false
%ArgV = ['-prg', Input, '-trace', TraceFile, -o, IntAutomata]
main(ArgV):-
    cleanup,
    setOptions(ArgV,Input,[ErrorTrace|_], OutS), %just considering the first error trace
    ppl_initialize,
    %yices_init,
    computeInterpolantAutomaton(Input,false,ErrorTrace),
    %yices_exit,
    write('The interpolant automaton is : '), nl,
    printFTA(ErrorTrace, OutS),
    close(OutS),
    ppl_finalize.

setOptions(ArgV,Input,Traces, OutS) :-
	get_options(ArgV,Options,_),
	(member(programO(Input),Options);
			write(user_output,'No input file given.'),nl(user_output)),
    (member(traces(PFile),Options), readErrorTrace(PFile, Traces);
                write(user_output,'No error trace file given.'),nl(user_output)),
    (member(generateAut(OutFile),Options), open(OutFile,append,OutS);
			OutS=user_output).

%assume there is only one trace in the error trace file
readErrorTrace(PFile, Traces):-
    open(PFile, read, S),
    readTerms(S, Traces),
    %read(S, counterexample(Trace)),
    close(S).

%some optionmisations here, if a clause is added do not evaluate other options at all
computeInterpolantAutomaton(F,A,CExTrace):-
    computeTreeInterpolants(F,A,CExTrace),
    yices_init,
    generateIFTA,
    yices_exit.

computeTreeInterpolants(F,A,CExTrace):-
    CExTrace=counterexample(Trace),
    load_file(F),
    interpolantTree(A,Trace,Tree, 1), % tree nodes begin with 1
    %write(Tree), nl,
    open('/tmp/interpolant.props', write, S),
    writeInterpolants(S,Tree),
    close(S).


generateIFTA:-
    readInterpolants,
    my_clause(H, Body, CId),
    separate_constraints(Body, Cs, B),
    varset((H,Body), Vars),
    numbervars(Vars,0,_),
    collectBodyInterpolants(B, BIs, InterpolantIds),
    append(Cs, BIs, BodyInterpolants),
    makeRealVars(Vars, YicesVars),
    (H=false ->
        yices_unsat((BodyInterpolants), YicesVars),
        makeFTA(H,B,CId, _, InterpolantIds)
    ;
        interpolant((H:-I-IId)),
        getNegHeadFormula(I, NI),
        append(NI, BodyInterpolants, ClsFormula),
        yices_unsat(ClsFormula, YicesVars),
        makeFTA(H,B,CId, IId, InterpolantIds)
    ),
    fail.
generateIFTA.



/*
generateIFTA:-
    readInterpolants,
    my_clause(H, Body, CId),
    separate_constraints(Body, Cs, B),
    varset((H,Body), Vars),
    numbervars(Vars,0,_),
    checkClsValidity(B, Cs, H, Vars, CId),
    fail.
generateIFTA.

checkClsValidity(Bs, Cs, H, Vars, CId):-
    collectBodyInterpolants(Bs, BIs),
    append(Cs, BIs, BodyInterpolants),
    makeRealVars(Vars, YicesVars),
    (H=false ->
        yices_unsat((BodyInterpolants), YicesVars),
        makeFTA(H,Bs,CId)
    ;
        collectDisjInterpolants([H], I),
        getNegHeadFormula(I, NI),
        append(NI, BodyInterpolants, ClsFormula),
        yices_unsat(ClsFormula, YicesVars),
        makeFTA(H,Bs,CId)
    ),!.
*/


writeInterpolants(S, tree(false,_,_,SubTrees,_, _, _)):-
    !,
    writeInterpolantTrees(S, SubTrees).
writeInterpolants(S, tree(B,_,I,SubTrees,_, _, NId)):-
    write(S, B),
    write(S, ' :- '),
    write(S, I),
    write(S,'-'),
    write(S, NId),
    write(S, '.'),
    %assertz(interpolant((B:-I))),
    nl(S),
    writeInterpolantTrees(S, SubTrees).
	
writeInterpolantTrees(_,[]).
writeInterpolantTrees(S, [B|Bs]) :-
	writeInterpolants( S, B),
    writeInterpolantTrees(S, Bs).

%rename each predicate to __r to avoid confusion except false which renames to error
makeFTA(H,Bs,Id, HId, InterpolantIds) :-
	functor(H,P,_),
	getPreds(Bs,Qs,InterpolantIds),
	LHS =.. [Id|Qs],
    (P=false ->
        assertFTATransition(LHS, errortrace)
    ;
        renameHead(P, RP, HId),
        assertFTATransition(LHS, RP)
    ),
     nl.

renameHead(P, P1, PId):-
    name(P, PName),
    name(PId, IdName),
    append(PName, [95,95,95|IdName], PName1), % ___PId
    name(P1, PName1).

getPreds([],[],[]).
getPreds([B|Bs],[P1|Qs],[Id|Ids]) :-
	functor(B,P,_),
    name(P, PName),
    name(Id, IdName),
    append(PName, [95,95,95|IdName], PName1), % ___Id
    name(P1, PName1),
	getPreds(Bs,Qs,Ids).

collectBodyInterpolants([], [],[]).
collectBodyInterpolants([B|Bs], Interpolants, [IId|RIds]):-
    interpolant((B:-I-IId)),
    collectBodyInterpolants(Bs, BIs, RIds),
    append(I, BIs, Interpolants).


collectDisjInterpolants([B], [BDInterpolants]):-
    !,
    findall(I, interpolant((B:-I)), BInterpolants), %BInterpolants is a list of a list
    listofList2Disj(BInterpolants, BDInterpolants).
collectDisjInterpolants([B|Bs], [BDInterpolants|BsDInterpolants]):-
    !,
    findall(I, interpolant((B:-I)), BInterpolants), %BInterpolants is a list of a list
    listofList2Disj(BInterpolants, BDInterpolants),
    collectDisjInterpolants(Bs, BsInterpolants),
    listofList2Disj(BsInterpolants, BsDInterpolants).
collectDisjInterpolants([], []).

readInterpolants:-
    open('/tmp/interpolant.props', read, S),
    read(S,C),
    storeInterpolants(S,C),
    close(S).

storeInterpolants(_, end_of_file):-!.
storeInterpolants(S, I):-
    assertz(interpolant(I)),
    read(S, I1),
    storeInterpolants(S, I1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Ys is the set of head variable and Zs the set of existial vars, 0 represents no interpolant
%is computed and K is the node Id
constraintTree(B,T,tree(B,Ys,Cs1,SubTrees,Zs, 0, K), K, K2) :-
	T =..[C|Ts1],
	my_clause(B,Bs1,C),
	B =..[_|Ys],
	varset((B,Bs1),Ws),
	setdiff(Ws,Ys,Zs),
	separate_constraints(Bs1,Cs1,Bs2),
    K1 is K+1,
	constraintTrees(Bs2,Ts1,SubTrees, K1, K2).
	
constraintTrees([],[],[], K, K).
constraintTrees([B|Bs],[T|Ts],[S|Ss], K, K2) :-
	constraintTree(B,T,S,K, K1),
    constraintTrees(Bs,Ts,Ss, K1, K2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%       BFS  Begin        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bfs(tree(_,_,_,[],_, _, K), [],RevBfsOrder,[K|RevBfsOrder]) :-
    !.

% Aux is the auxillary list
bfs(tree(_,_,_,SubTrees,_, _, K), Unprocessed, RevBfsOrder,RevBfsOrder1) :-
    append(Unprocessed, SubTrees, SubTrees1),
	bfsChildren(SubTrees1,  [K|RevBfsOrder], RevBfsOrder1).
	
bfsChildren([],  RevBfsOrder, RevBfsOrder).
bfsChildren([S|Ss], RevBfsOrder, RevBfsOrder2) :-
	bfs(S, Ss, RevBfsOrder, RevBfsOrder2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      BFS     End      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%update interpolant for the false node
updateInterpolantForFalseNode(tree(false,Ys,_,SubTrees,Zs, _, K), tree(false,Ys,false,SubTrees,Zs, 1, K)).

updateInterpolantTree(tree(B,Ys,_,SubTrees,Zs, _, K), K, I, tree(B,Ys,I,SubTrees,Zs, 1, K)) :-
	!.
updateInterpolantTree(tree(B,Ys,Cs1,SubTrees,Zs, IsInterpolant, K), N, I, tree(B,Ys,Cs1,SubTrees1,Zs, IsInterpolant, K)) :-
	!,
    updateInterpolantTrees(SubTrees, N,I, SubTrees1).

	
updateInterpolantTrees([],_,_, []).
updateInterpolantTrees([B|Bs],N,I, [B1|Bs1]) :-
	updateInterpolantTree(B,N,I, B1),
    updateInterpolantTrees(Bs,N,I, Bs1).

getInterpolantTree(_, CTree, [], CTree):-
    !.
%for node1 which is the false node, do nothing
getInterpolantTree(Xs, CTree, [1|Ns], ITree):-
    !,
    getInterpolantTree(Xs, CTree, Ns, ITree).
getInterpolantTree(Xs, CTree, [N|Ns], ITree):-
    interpolantConstituents(CTree, N, [], A,    [], B ), %the first [] is for collecting constraits for A and the second [] for collecting constraints for B
    %write('for node '), write(N), nl,
    %write('Interpolant of '), write(A), nl,
    %write(' and '), nl,
    %write(B), nl,
    computeInterpolant(Xs, A, B, I),
    %write('interpolant is '), nl,
    %write(I), nl,
    updateInterpolantTree(CTree, N, I, CTree1),
    getInterpolantTree(Xs, CTree1, Ns, ITree).



%if the node is found, then its constraints along with the interpolants of its children becomes the first constraint
interpolantConstituents(tree(_,_,Cs1,SubTrees,_, _, K), K, First1, First,   Second1, Second):-
    !,
    append(Cs1,First1, First2),
    collectConstraintsA(SubTrees,K, First2, First,  Second1, Second).
%if the node is not found, all the constraints go into the second
%if a child node has interpolant already do not continue to its children, stop there
interpolantConstituents(tree(_,_,Cs1,_,_, 1, _),  _, First, First, InitConstr, Second):-
    !,
    append(Cs1,InitConstr, Second).
%if a child node has no interpolant then continue to its children
interpolantConstituents(tree(_,_,Cs1,SubTrees,_, 0, _), N, First1, First,InitConstr, Second):-
    !,
    append(Cs1,InitConstr, Second1),
    collectConstraints(SubTrees,N, First1, First,  Second1, Second).


collectConstraintsA([],_,First, First, Second,Second):-
    !.
collectConstraintsA([T|Ts],N, First1,First,  InitConstr, Second) :-
	interpolantConstituentsA(T,N, First1, First2,  _, _),
	collectConstraintsA(Ts,N, First2, First,  InitConstr, Second).

%this is done with the view that all the children nodes have their interpolant computed already
interpolantConstituentsA(tree(_,_,Cs1,_,_, _, _), _, First1, First2,  _, _):-
    !,
    append(Cs1,First1, First2).
    %collectConstraintsA(SubTrees,_, First2, First,  Second1, Second).

collectConstraints([],_,First, First, Second,Second):-
    !.
collectConstraints([T|Ts],N, First1, First, InitConstr, Second) :-
	interpolantConstituents(T,N, First1, First2,  InitConstr, Second1),
	collectConstraints(Ts,N, First2,First, Second1, Second).

interpolantTree(A,Trace,ITree, K) :-
	constraintTree(A,Trace,Tree1, K, _),
    %write('constraint tree '), nl,
    varset(Tree1,Xs),
	numbervars(Xs,0,_),
    %write(Tree1), nl,
    bfs(Tree1, [], [], List),
    %write('bfs list '), nl,
    %write(List), nl,
    getInterpolantTree(Xs,Tree1, List, ITree1),
    %write('int tree '), nl,
    %write(ITree1), nl,
    %update interpolant for the false node
    updateInterpolantForFalseNode(ITree1, ITree).


printFTA(Trace, S):-
    %check if interpolant FTA is not empty
    findall(Left, ftaTransition((Left :- _)), FTAL),
    (FTAL=[] ->
        %print FTA corresponding to the error trace
        printErrorFTA(Trace, S)
    ;
        printInterpolantFTA(Trace, S)
    ).

printErrorFTA(Trace, S):-
    makeTraceFTAs([Trace],0,_,S).
printErrorFTA(_,_).

printInterpolantFTA(_,S):-
    ftaTransition((Left :- Right)),
    write(S, Left),
    write(S, ' -> '),
    write(S, Right),
    write(S,'.'),
    nl(S),
    fail.
printInterpolantFTA(_,_).

assertFTATransition(Left, Right):-
    (ftaTransition((Left :- Right)) ->
        true
    ;
        assertz(ftaTransition((Left :- Right)))
    ).


readTerms(S,Ts) :-
	read(S,C),
	(C==end_of_file -> Ts=[];
	 Ts=[C|Ts1],
	 readTerms(S,Ts1)).

makeTraceFTAs([T|Ts],K0,K2,OutS) :-
	T=counterexample(Trace),
	!,
	term2type4(Trace,[(L->_)|FTA],K0,K1),
	writeFTA([(L->errortrace)|FTA],OutS),
	makeTraceFTAs(Ts,K1,K2,OutS).
makeTraceFTAs([_|Ts],K0,K1,OutS) :-
	makeTraceFTAs(Ts,K0,K1,OutS).
makeTraceFTAs([],K,K,_).

term2type(A,Def) :-
	term2type6(A,_,Def,[],0,_).
	
term2type4(A,Def,K0,K1) :-
		term2type6(A,_,Def,[],K0,K1).
	
termlist2type(Xs,T) :-
	argtypes(Xs,_,T,[],0,_).


term2type6(T, dynamic,As,As,K,K) :-
        var(T),
        !.
term2type6(T, R,[(L -> R)|As],As1,K,K2) :-
        newname(K,R),
        K1 is K+1,
        T =.. [F|Xs],
        argtypes(Xs,Qs,As,As1,K1,K2),
        L =.. [F|Qs].

argtypes([],[],As,As,K,K).
argtypes([T|Ts],[Q|Qs],As0,As2,K0,K2) :-
        term2type6(T,Q,As0,As1,K0,K1),
        argtypes(Ts,Qs,As1,As2,K1,K2).
        
newname(K,P) :-
	name(K,KN),
	append("errortrace",KN,QKN),
	name(P,QKN).

writeFTA([],_).
writeFTA([T|Ts],S) :-
	write(S,T),
	write(S,'.'),
	nl(S),
	writeFTA(Ts,S).


cleanup:-
    retractall(interpolant(_)),
    retractall(my_clause(_,_,_)),
    retractall(ftaTransition(_)).


getNegHeadFormula([HF], HF1):-
        HF1=[neg(HF)].



