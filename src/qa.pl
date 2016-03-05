:- module(qa, [main/1, 
	qaInvoke/4,
	answerClauses/3,
	queryClauses/3,
	queryAnsClauses/3,
	indexQueryAnsClauses/3,
	indexQueryClauses/3,
	addInitQueries/3,
	addIndexInitQueries/3,
	makeIndexedAtom/5], []).

:- use_module(library(read)).
:- use_module(library(write)).
:- use_module(library(lists)).

:- use_module(chclibs(builtins)).
:- use_module(chclibs(readprog)).
:- use_module(chclibs(canonical)).
:- use_module(chclibs(myterms)).
:- use_module(chclibs(flatnames)).

:- include(chclibs(common)).

%% usage:  qa Infile -q "Query" [-o Outfile][-ans][-index][-builtin] 
%% qa pumpStates1.pl -q "pumpSystem" -o qpumpStates.pl
%% qa pumpStates1.pl -q "pumpSystem" -o qpumpStates.pl -ans
%% qa pumpStates1.pl -q "pumpSystem" -o qpumpStates.pl -ans -index
%% qa mc91.pl -q "ineed(_,_)" -index

%% Default output = user_output
%% Default options noans, noindex, nobuiltin

%% E.g. previous version called with
%% qa "Query" outfile
%% would now be called as
%% qa -q "Query" -o outfile 

%% The options are interpreted as follows

%% -ans :  generates answer predicates such as rev_ans(X,Y) for rev(X,Y)
%% -index : generates a different query for each occurrence of a body call
%% -builtin : generates calls for builtins

%% Default output (if -o outfile is not present) = user_output
%% Default options noans, noindex, nobuiltin
	
	
% Possible extension - add a version where only calls are of interest.  
% The answers to predicates that only appear as the last call in clause
% bodies are not needed.

recognised_option('-index',index,[]).
recognised_option('-ans',ans,[]).
recognised_option('-builtin',builtin,[]).
recognised_option('-query',query(Q),[Q]).
recognised_option('-q',query(Q),[Q]).
recognised_option('-o',output_file(F),[F]).
recognised_option('-right',right,[]).

main(ArgV) :-
	get_options(ArgV,Options,[F|_]),
	readprog(F,[_|Cs]),
	getQuery(Options,Q1),
	outfileStream(Options,S),
	qaInvoke(Cs,Q1,Options,Qs),
	writeClauses(Qs,S),
	close(S).
	
outfileStream(Options,S) :-
	member(output_file(F1),Options),
	!,
	open(F1,write,S).
outfileStream(_,user_output).
   
getQuery(Options,[Q1]) :-
	member(query(Q),Options),
	!,
	convertQueryString(Q,Q1).
getQuery(_,[]). 	% no query supplied

qaInvoke(Cls,Q1,Options,Qs0) :-
	member(ans,Options) ->
	   (member(index,Options) ->
	      (member(builtin,Options) ->
	        qaInvoke111(Cls,Q1,Qs0);   % ans, index, builtin
	        qaInvoke110(Cls,Q1,Qs0));  % ans, index, nobuiltin
	      (member(builtin,Options) ->
	        qaInvoke101(Cls,Q1,Qs0);   % ans, noindex, builtin
	        qaInvoke100(Cls,Q1,Qs0))); % ans, noindex, nobuiltin
	   (member(index,Options) ->
	      (member(builtin,Options) ->
	        qaInvoke011(Cls,Q1,Qs0);   % noans, index, builtin
	        qaInvoke010(Cls,Q1,Qs0));  % noans, index, nobuiltin
	      (member(builtin,Options) ->
	        qaInvoke001(Cls,Q1,Qs0);   % noans, noindex, builtin
	        qaInvoke000(Cls,Q1,Qs0))). % noans, noindex, nobuiltin
	        
	        
qaInvoke111(Cls,Q,Qs0) :-
	answerClauses(Cls,Qs0,Qs1),
	makeQueryClauses(Cls,Qs1,Qs2,1,[index,ans,builtin]),
	addQueryClauses(Q,Qs2,[],[index]).
qaInvoke110(Cls,Q,Qs0) :-
	answerClauses(Cls,Qs0,Qs1),
	makeQueryClauses(Cls,Qs1,Qs2,1,[index,ans,nobuiltin]),
	addQueryClauses(Q,Qs2,[],[index]).
qaInvoke101(Cls,Q,Qs0) :-
	answerClauses(Cls,Qs0,Qs1),
	makeQueryClauses(Cls,Qs1,Qs2,1,[noindex,ans,builtin]),
	addQueryClauses(Q,Qs2,[],[noindex]).
qaInvoke100(Cls,Q,Qs0) :-
	answerClauses(Cls,Qs0,Qs1),
	makeQueryClauses(Cls,Qs1,Qs2,1,[noindex,ans,nobuiltin]),
	addQueryClauses(Q,Qs2,[],[noindex]).
qaInvoke011(Cls,Q,Qs0) :-
	programClauses(Cls,Qs2),
	makeQueryClauses(Cls,Qs0,Qs1,1,[index,noans,builtin]),
	addQueryClauses(Q,Qs1,Qs2,[index]).
qaInvoke010(Cls,Q,Qs0) :-
	programClauses(Cls,Qs2),
	makeQueryClauses(Cls,Qs0,Qs1,1,[index,noans,nobuiltin]),
	addQueryClauses(Q,Qs1,Qs2,[index]).
qaInvoke001(Cls,Q,Qs0) :-
	programClauses(Cls,Qs2),
	makeQueryClauses(Cls,Qs0,Qs1,1,[noindex,noans,builtin]),
	addQueryClauses(Q,Qs1,Qs2,[noindex]).
qaInvoke000(Cls,Q,Qs0) :-
	programClauses(Cls,Qs2),
	makeQueryClauses(Cls,Qs0,Qs1,1,[noindex,noans,nobuiltin]),
	addQueryClauses(Q,Qs1,Qs2,[noindex]).
	
qa(F1,Q,F2) :-
	readprog(F1,[_|Cs]), 
	answerClauses(Cs,As,Qs0),
	queryAnsClauses(Cs,Qs0,[]), 
	addInitQueries([Q],Qs1,As),
	open(F2,write,S),
	writeClauses(Qs1,S),
	close(S).
	
queryClauses(Cls,QCls0,QCls1) :-
	makeQueryClauses(Cls,QCls0,QCls1,1,[noindex,noans,builtin]).
	
queryAnsClauses(Cls,QCls0,QCls1) :-
	makeQueryClauses(Cls,QCls0,QCls1,1,[noindex,ans,builtin]).
	
indexQueryAnsClauses(Cls,QCls0,QCls1) :-
	makeQueryClauses(Cls,QCls0,QCls1,1,[index,ans,builtin]).
	
indexQueryClauses(Cls,QCls0,QCls1) :-
	makeQueryClauses(Cls,QCls0,QCls1,1,[index,noans,builtin]).
	


programClauses([predicates(_)|Cs],Cs) :-
	!.
programClauses(Cs,Cs).

answerClauses([],QProg,QProg).
answerClauses([predicates(_)|Cs],Out0,Out1) :- 
	!,
	answerClauses(Cs,Out0,Out1).
answerClauses([C|Cs],[clause((A1 :- QB),Vs)|Out0],Out1) :- 
	melt(C,clause((A:-B),Vs)),
	flat_name(ans(A),A1),
	flat_name(query(A),QA),
	bodyAns(B,B1,[_,ans|_]),
	qbody(QA,B1,QB),
	prettyvars(clause((A1 :- QB),Vs)),
	answerClauses(Cs,Out0,Out1).
	
addInitQueries(Q,Cls0,Cls1) :-
	addQueryClauses(Q,Cls0,Cls1,[noindex]).
	
addIndexInitQueries(Q,Cls0,Cls1) :-
	addQueryClauses(Q,Cls0,Cls1,[index]).
	
%%%%%%% local predicates

makeQueryClauses([],QProg,QProg,_,_).
makeQueryClauses([predicates(_)|Cs],Out,Out2,N,Options) :- 
	makeQueryClauses(Cs,Out,Out2,N,Options).
makeQueryClauses([clause(C,Vs)|Cs],Out,Out2,N,Options) :- 
	melt(clause(C,Vs),clause(C1,_Vs1)),
	makeQAclauses(clause(C1,Vs),Out,Out1,N,Options),
	N1 is N+1,
	makeQueryClauses(Cs,Out1,Out2,N1,Options).
	
makeQAclauses(clause(Cl,Vs),Out,Out1,I,Options) :- 
	!,
	headBody(Cl,A,B),	% allows for Logen or other representations 
	bodylitqueries(A,B,true,Vs,Out,Out1,I,1,Options).
makeQAclauses(_,Out,Out,_,_).

bodylitqueries(_,true,_,_,Out,Out,_,_,_) :-
	!.
bodylitqueries(A,B,Pre,Vs,Out0,Out3,I,J,Options) :-
	removefirst(B,Bj,Bs),
	!,
	bodyAtom(Bj,Bj1),
	checkMemoed(Bj,Bj2),
	joingoals(Pre,Bj2,Pre1),
	bodyAns(Pre,PreAns,Options),
	flat_name(query(A),QA),
	qbody(QA,PreAns,QPre),
	makeIndexedAtomClause(Bj1,J,I,QPre,Vs,QueryBIJ,Out0,Out1,Options),
	makeQueryLinkClause(QueryBIJ,Bj1,Out1,Out2,Options),
	J1 is J+1,
	bodylitqueries(A,Bs,Pre1,Vs,Out2,Out3,I,J1,Options).

makeIndexedAtomClause(B,_,_,_,_,B,Out0,Out0,[_,_,nobuiltin]) :-
	builtin(B),
	!.
makeIndexedAtomClause(B,I,J,QPre,Vs,QueryBIJ,[BjClause|Out0],Out0,[index|_]) :-
	makeIndexedAtom(B,'QUERY',I,J,QueryBIJ),
	copyterm(clause((QueryBIJ :- QPre),Vs),BjClause),
	prettyvars(BjClause).
makeIndexedAtomClause(B,_,_,QPre,Vs,QB,[BjClause|Out0],Out0,[noindex|_]) :-
	flat_name(query(B),QB),
	copyterm(clause((QB :- QPre),Vs),BjClause),
	prettyvars(BjClause).
	
makeIndexedAtom(B,Tag,I,J,TagBIJ) :-
	TagB =.. [Tag,B],
	flat_name(TagB,QBj),
	QBj =.. [Q1|Xs],
	QI =.. [Q1,I],
	flat_name(QI, QBjI),
	QIJ =.. [QBjI,J],
	flat_name(QIJ,QBIJ),
	TagBIJ =.. [QBIJ|Xs].
	
makeQueryLinkClause(B,_,Out0,Out0,[_,_,nobuiltin]) :-
	builtin(B),
	!.
makeQueryLinkClause(QueryBIJ,Bj1,[QClause|Out0],Out0,[index|_]) :-
	QueryBIJ =.. [QBIJ|_],
	flat_name(query(Bj1),QBNoIndex),
	functor(QBNoIndex,Q,M),
	functor(Query,Q,M),
	Query =.. [Q|Ys],
	QueryB =.. [QBIJ|Ys],
	QClause = clause((Query:- QueryB),[]),
	prettyvars(QClause).
makeQueryLinkClause(_,_,Out0,Out0,[noindex|_]).

qbody(QA,true,QA) :-
	!.
qbody(QA,Pre,(QA,Pre)).
	

	
bodyAns(true,true,_) :-
	!.
bodyAns((B,Bs),(B,Bs1),Sel) :-
	builtin(B),
	!,
	bodyAns(Bs,Bs1,Sel).
bodyAns((B,Bs),(B1,Bs1),[I,ans|Os]) :-
	!,
	flat_name(ans(B),B1),
	bodyAns(Bs,Bs1,[I,ans|Os]).
bodyAns((B,Bs),(B,Bs1),[I,noans|Os]) :-
	!,
	bodyAns(Bs,Bs1,[I,noans|Os]).
bodyAns(B,B,_) :-
	builtin(B),
	!.
bodyAns(B,B1,[_,ans|_]) :-
	!,
	flat_name(ans(B),B1).
bodyAns(B,B,[_,noans|_]).

addQueryClauses([],Csl,Csl,_).
addQueryClauses([Q|Qs],Cls0,Cls2,Options) :-
	addQueryClause(Q,Cls0,Cls1,Options),
	addQueryClauses(Qs,Cls1,Cls2,Options).
	
addQueryClause(Q,QCls0,QCls2,Options) :-
	makeIndexedAtomClause(Q,0,0,true,[],Q00,QCls0,QCls1,Options),
	makeQueryLinkClause(Q00,Q,QCls1,QCls2,Options).


headBody((logen(_,H) :- B),H,B) :-  % allow for logen form
	!.
headBody((H :- B),H,B).

removefirst((B,Bs),B,Bs) :-
	!.
removefirst(B,B,true).

bodyAtom(logen(_,B),B) :-  % allow for logen form
	!.
bodyAtom(B,B).

checkMemoed(logen(memo,_),true) :- 	% omit answers for Logen memoed calls 
	!.
checkMemoed(logen(rescall,_),true) :-  % omit answers for Logen rescalled calls 
	!.
checkMemoed(logen(_,B),B) :-
	!.
checkMemoed(B,B).


joingoals(true,Xs,Xs) :-
        !.
joingoals(Xs,true,Xs) :-
        !.
joingoals((true,Xs),Ys,Zs) :-
        !,
        joingoals(Xs,Ys,Zs).
joingoals((Xs,true),Ys,Zs) :-
        !,
        joingoals(Xs,Ys,Zs).
joingoals((X,Xs),Ys,(X,Zs)) :-
        !,
        joingoals(Xs,Ys,Zs).
joingoals(X,Xs,(X,Xs)) :-
        X =.. [F|_],
        F \== ','.

:- use_module(library(system), [system/1]).

convertQueryString(Q,Q1) :-
        open('/tmp/querystring',write,S),
        write(S,Q),
        write(S,'.'),
        nl(S),
        close(S),
        open('/tmp/querystring',read,S1),
        read(S1,Q1),
        close(S1),
        system('rm /tmp/querystring').

