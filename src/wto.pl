:- module(wto, [main/1,wto/6,wto_file/3], []).

% Finding widening points.  See Bourdoncle 1993.

% Changes: depth+breadth first travesal
%          Only add wideningpoint if no widening point exists on path

:- use_module(library(lists)).
:- use_module(library(write)).
:- use_module(library(aggregates)).
:- use_module(balanced_tree).
:- use_module(builtins).
:- use_module(setops).
:- use_module(readprog).

main([F]) :-
	readprog(F,Cls),
	sortClauses(Cls,Ps,Prog),
    write('Depth first'),nl,
	wto(dfs,feedback,Ps,Prog,_Wtod1,WPsDfs1),
	makeset(WPsDfs1,WPsd1),
	open('wideningpoints_dfs_fb.pl',write,Sd1),
	writeTerms(WPsd1,Sd1),
    close(Sd1),
%	wto(dfs,alh,Ps,Prog,_Wtod2,WPsDfs2),
%	makeset(WPsDfs2,WPsd2),
%	open('wideningpoints_dfs_alh.pl',write,Sd2),
%	writeTerms(WPsd2,Sd2),
%        close(Sd2),
%	wto(dfs,alt,Ps,Prog,_Wtod3,WPsDfs3),
%	makeset(WPsDfs3,WPsd3),
%	open('wideningpoints_dfs_alt.pl',write,Sd3),
%	writeTerms(WPsd3,Sd3),
%        close(Sd3),
%	wto(dfs,alme,Ps,Prog,_Wtod4,WPsDfs4),
%	makeset(WPsDfs4,WPsd4),
%	open('wideningpoints_dfs_alme.pl',write,Sd4),
%	writeTerms(WPsd4,Sd4),
%        close(Sd4),

    write('Breadth first'),nl,
%	wto(bfs,feedback,Ps,Prog,_Wtob1,WPsBfs1),
%	makeset(WPsBfs1,WPsb1),
%	open('wideningpoints_bfs_fb.pl',write,Sb1),
%	writeTerms(WPsb1,Sb1),
%        close(Sb1),
%	wto(bfs,alh,Ps,Prog,_Wtob2,WPsBfs2),
%	makeset(WPsBfs2,WPsb2),
%	open('wideningpoints_bfs_alh.pl',write,Sb2),
%	writeTerms(WPsb2,Sb2),
%        close(Sb2),
%	wto(bfs,alt,Ps,Prog,_Wtob3,WPsBfs3),
%	makeset(WPsBfs3,WPsb3),
%	open('wideningpoints_bfs_alt.pl',write,Sb3),
%	writeTerms(WPsb3,Sb3),
%        close(Sb3),
	wto(dfs,alme,Ps,Prog,_Wtob4,WPsBfs4),
	makeset(WPsBfs4,WPsb4),
	open('wideningpoints_bfs_alme.pl',write,Sb4),
	writeTerms(WPsb4,Sb4),
        close(Sb4).


  %  write('Bredth first'),nl,
  %      wto(bfs,Ps,Prog,_Wtob,WPsBfs),
  %      makeset(WPsBfs,WPsb), 
%	open('wideningpoints_bfs.pl',write,Sb),
%	writeTerms(WPsb,Sb),
%       close(Sb),
%    chooseMin(WPsd,WPsb,WPs),
%	open('wideningpoints.pl',write,S),
%	writeTerms(WPs,S),
%	close(S).

chooseMin(W1,W2,W2) :-
    length(W1,X),
    length(W2,Y),
    X >= Y,!.
chooseMin(W1,_,W1).

% wto(Prog,Cs):  Ps is a list of predicates (output of readprog)




wto_file(F,_,WP) :-
        readprog(F,Cls),
        sortClauses(Cls,Ps,Prog),
        wto(bfs,feedback,Ps,Prog,_Wto,WPs1),
        %wto(bfs,alme,Ps,Prog,_Wto,WPs1),
        makeset(WPs1,WPs),
        %writeWto(Wto,user_output),
        open(WP,write,S),
        writeTerms(WPs,S),
        close(S).




wto(TM,alme,Ps,Prog,_Wto,WPs) :-
    !,
	make_callgraph(Ps,Prog,G),
	traversekey_tree(G,Nodes),
	dfsSweep(TM,alme,Nodes,G,[],root,_,[],_Stack,[],Ls),
%WPs should now containt a list of lists
%Each list is a "loop" - cut all loops picking lowest number of widen.p. 
    cutloops(Ps,Ls,WPs).
%    write(WPs),nl.
%	parenthesise(Stack,G,[],Wto).


wto(TM,feedback,Ps,Prog,Wto,WPs) :-
        make_callgraph(Ps,Prog,G),
        traversekey_tree(G,Nodes),
        firstdfsSweep(TM,feedback,Nodes,G,[],root,_,[],Stack,[],WPs),
%write('Feedback WPs '),write(WPs),nl,
        parenthesise(Stack,G,[],Wto).


cutloops(_,[],[]).
cutloops(Ps,Ls,[widening_point(Phdg,Degree)|WPs]) :-
    bagof(X,(member(loop(X),Ls)),LsPs),
    %write('Found loops'),nl,
    findHighestDegreeNode(Ps,LsPs,(empty,0),Phdg,Degree),
%    write('Highest degree '),write(Phdg),write(' dgr '),write(Degree),nl,
    %Now append it to wideningpoint list
    %Now remove loops that have been cut    
    remcutloops(Phdg,Ls,LsCut),

  %  length(Ls,X1),
  %  length(LsCut,X2),
    %write((X1,X2)),nl,
    !,
    cutloops(Ps,LsCut,WPs).
    

remcutloops(_,[],[]).
remcutloops(P,[loop(L)|Ls],LsC) :-
% write((P,L)),nl,
    member(P,L),
    !,
    remcutloops(P,Ls,LsC).
remcutloops(P,[L|Ls],[L|LsC]) :-
    !,
    remcutloops(P,Ls,LsC).

findHighestDegreeNode([],_,(P,Degree),P,Degree).
findHighestDegreeNode([P|Ps],Ls,(_,Dgr),Phdg,Degree) :-
%    write((P,Ls)),nl,
    bagof(L,(member(L,Ls),member(P,L)),Ploops),
%    write('Bagof done'),nl,
    length(Ploops,Len),
    Len >= Dgr,
    !,
    findHighestDegreeNode(Ps,Ls,(P,Len),Phdg,Degree).
findHighestDegreeNode([P|Ps],Ls,(Pc,Dgr),Phdg,Degree) :-
    bagof(L,(member(L,Ls),member(P,L)),Ploops),
    length(Ploops,Len),
    Len < Dgr,
    !,
    findHighestDegreeNode(Ps,Ls,(Pc,Dgr),Phdg,Degree).
findHighestDegreeNode([_|Ps],Ls,(Pc,Dgr),Phdg,Degree) :-
    findHighestDegreeNode(Ps,Ls,(Pc,Dgr),Phdg,Degree).


firstdfsSweep(_,_,[], _, _, MarkList, MarkList, Stack, Stack,Ws,Ws) :- write('End of sweep'),nl.
firstdfsSweep(TM,WM,[N|Ns], Graph, As, MarkListIn, MarkListOut, StackIn, StackOut,Ws0,Ws2) :-
        search_tree(MarkListIn,N,black),   % N already visited
        !,
        firstdfsSweep(TM,WM,Ns, Graph, As,MarkListIn, MarkListOut, StackIn, StackOut,Ws0,Ws2).
firstdfsSweep(dfs,WM,[N|Ns], Graph, As,MarkListIn, MarkListOut, StackIn, StackOut,Ws0,Ws2) :-
        dfsNode(dfs,WM,Graph, N, As,MarkListIn, MarkListMid, StackIn, StackMid,Ws0,Ws1),
        firstdfsSweep(dfs,WM,Ns, Graph, As,MarkListMid, MarkListOut, StackMid, StackOut,Ws1,Ws2).
firstdfsSweep(bfs,WM,[N|Ns], Graph, As,MarkListIn, MarkListOut, StackIn, StackOut,Ws0,Ws2) :-
        dfsNode(dfs,WM,Graph, N, As,MarkListIn, MarkListMid, StackIn, StackMid,Ws0,Ws1),
        firstdfsSweep(bfs,WM,Ns, Graph, As,MarkListMid, MarkListOut, StackMid, StackOut,Ws1,Ws2).


dfsSweep(_,_,[], _, _, MarkList, MarkList, Stack, Stack,Ws,Ws).
dfsSweep(TM,WM,[N|Ns], Graph, As, MarkListIn, MarkListOut, StackIn, StackOut,Ws0,Ws2) :-
        search_tree(MarkListIn,N,black),   % N already visited
        !,
        dfsSweep(TM,WM,Ns, Graph, As,MarkListIn, MarkListOut, StackIn, StackOut,Ws0,Ws2).

%This is for the depth-first
dfsSweep(dfs,WM,[N|Ns], Graph, As,MarkListIn, MarkListOut, StackIn, StackOut,Ws0,Ws2) :-
        dfsNode(dfs,WM,Graph, N, As,MarkListIn, MarkListMid, StackIn, StackMid,Ws0,Ws1),
        dfsSweep(dfs,WM,Ns, Graph, As,MarkListMid, MarkListOut, StackMid, StackOut,Ws1,Ws2).

%This is for the bredth-first search
dfsSweep(bfs,WM,[N|Ns], Graph, As,MarkListIn, MarkListOut, StackIn, StackOut,Ws0,Ws2) :-
        dfsSweep(bfs,WM,Ns, Graph, As,MarkListIn, MarkListMid, StackIn, StackMid,Ws0,Ws1),
        dfsNode(bfs,WM,Graph, N, As,MarkListMid, MarkListOut, StackMid, StackOut,Ws1,Ws2).

dfsNode(TM,WM,Graph,N,As,M0,M2,S0,S1,Ws0,Ws1) :-
        insert_tree(M0,N,black,M1),   % mark node as visited
        find_succs(Graph,N,SuccList),
        dfs_each(TM,WM,SuccList,Graph,[N|As],N,M1,M2,S0,S1,Ws0,Ws1).

find_succs(Graph,N,SuccList) :-
        search_tree(Graph,N,links(SuccList,_)),
        !.
find_succs(_,_,[]).

find_preds(Graph,N,PredList) :-
        search_tree(Graph,N,links(_,PredList)),
        !.
find_preds(_,_,[]).



dfs_each(_,_,[],_,_,Par,M,M,S,[Par|S],Ws,Ws).
dfs_each(TM,WM,[N|Ns],G,As,Par,M0,M1,S0,S1,Ws0,Ws2) :-
        search_tree(M0,N,black),
        !,
	wideningPoint(WM,G,N,As,Ws0,Ws1),
        dfs_each(TM,WM,Ns,G,As,Par,M0,M1,S0,S1,Ws1,Ws2).

%This is for the depth first
dfs_each(dfs,WM,[N|Ns],G,As,Par,M0,M2,S0,S2,Ws0,Ws2) :-
        dfsNode(dfs,WM,G,N,As,M0,M1,S0,S1,Ws0,Ws1),
        dfs_each(dfs,WM,Ns,G,As,Par,M1,M2,S1,S2,Ws1,Ws2).

%This is for the bredth first
dfs_each(bfs,WM,[N|Ns],G,As,Par,M0,M2,S0,S2,Ws0,Ws2) :-
    dfs_each(bfs,WM,Ns,G,As,Par,M0,M1,S0,S1,Ws0,Ws1),
    dfsNode(bfs,WM,G,N,As,M1,M2,S1,S2,Ws1,Ws2).

wideningPoint(alme,_G,N,As,Ws0,[loop(ALC)|Ws0]) :-
	member(N,As),
 !,
    %Find part of ancestry list that forms the loop
    loopFromAncestrylist(N,As,ALC).

wideningPoint(feedback,_,N,As,Ws0,[widening_point(N)|Ws0]) :-
        member(N,As),
        !.

wideningPoint(_,_,_,_,Ws0,Ws0).


loopFromAncestrylist(N,[X|As],[X|ALC]) :-
    N \== X,
    loopFromAncestrylist(N,As,ALC).
loopFromAncestrylist(N,[N|_],[N]).

maxSuccAsN(G,N,As,ME) :-
    maxSuccAsNloop(G,N,As,0,_,_,ME).

%Note N is in As, so 3 arg. cannot be '[]'
maxSuccAsNloop(G,N,[N|_As],Min,_MinE,L1,N) :-
    find_preds(G,N,Sl),
    length(Sl,L1),
    L1 > Min,!.
maxSuccAsNloop(G,N,[N|_As],Min,MinE,Min,MinE) :-
    find_preds(G,N,Sl),
    length(Sl,L1),
    L1 =< Min,!.
maxSuccAsNloop(G,N,[X|As],Min,MinE,Mout,MoutE) :-
    N \== X,
    find_preds(G,X,Sl),
    length(Sl,L1),
    L1 =< Min,!,
    maxSuccAsNloop(G,N,As,Min,MinE,Mout,MoutE).
maxSuccAsNloop(G,N,[X|As],Min,_MinE,Mout,MoutE) :-
    N \== X,
    find_preds(G,X,Sl),
    length(Sl,L1),
    L1 > Min,!,
    maxSuccAsNloop(G,N,As,L1,X,Mout,MoutE).


noExistingWPancestry(_,[],_).
noExistingWPancestry(N,[A|As],Ws0) :-
    N \== A,
    noNinWs(A,Ws0),
    noExistingWPancestry(_X,As,Ws0).
noExistingWPancestry(N,[N|_As],Ws0) :-
    noNinWs(N,Ws0).

noNinWs(N,Ws0) :-
    memb1(widening_point(N),Ws0),
    !,
    fail.
noNinWs(_,_).

noAsInWs([],_).
noAsInWs([A|_As],Ws0) :-
    memb1(widening_point(A),Ws0),
    !,
    fail.
noAsInWs([_|As],Ws0) :-
    noAsInWs(As,Ws0).

% starting from a list of predicates, 
% make an adjacency list representation of the call graph 
% and the transposed call graph (reversing links).

make_callgraph([],_,root).
	
make_callgraph([P|Ps],Prog,G) :-
	make_callgraph(Ps,Prog,G1),
	!,
	immed_depends(Prog,P,[],Es),	% could be optimised by using tree instead of list
	%write('Start forward links '), write(P),nl,
	insert_forward_links(G1,P,Es,G2),
	%write('Start backward links'),nl,
	insert_back_links(Es,P,G2,G).

insert_forward_links(G1,P,Es,G2) :-
	search_replace_tree(G1,P,links(_,Ss),G2,links(Es,Ss)),
	!.
insert_forward_links(G1,P,Es,G2) :-
	insert_tree(G1,P,links(Es,[]),G2).

insert_back_links([],_,G,G).
insert_back_links([Q|Qs],P,G0,G2) :-
	search_replace_tree(G0,Q,links(Ps,Ss),G1,links(Ps,Ss1)),
	!,
	setunion([P],Ss,Ss1),
	insert_back_links(Qs,P,G1,G2).
insert_back_links([Q|Qs],P,G0,G2) :-
	insert_tree(G0,Q,links([],[P]),G1),
	insert_back_links(Qs,P,G1,G2).

immed_depends(Prog,P/N,Rs,Rs1) :-
	user_clauses(Prog,P/N,Cls),
	body_preds(Cls,Rs,Rs1).

body_preds([(_ :- B)|Cs],S,S2) :-	
	bodylits(B,S,S1),
	body_preds(Cs,S1,S2).
body_preds([],S,S).


bodylits(domainBody(B,BD,HD),S,S3) :- 	% to allow for domain programs
	!,
	bodylits(B,S,S1),
	bodylits(BD,S1,S2),
	bodylits(HD,S2,S3).
bodylits((B,Bs),S,S2) :-
	\+ sp_builtin(B),
	!,
	functor(B,T,N),
	insertp(T/N,S,S1),
	bodylits(Bs,S1,S2).
bodylits((_,Bs),S,S1) :-
	!,
	bodylits(Bs,S,S1).
bodylits(B,S,S1) :-
	\+ sp_builtin(B),
	!,
	functor(B,T,N),
	insertp(T/N,S,S1).
bodylits(_,S,S).

insertp(X,L,L) :-
	memb1(X,L),
	!.
insertp(X,L,[X|L]).

%insertp(X,[],[X]).
%insertp(X,[X|Xs],[X|Xs]) :-
%	!.
%insertp(X,[Y|Xs],[Y|Ys]) :-
%	insertp(X,Xs,Ys).


memb1(X,[X|_]) :-
	!.
memb1(X,[_|Xs]) :-
	memb1(X,Xs).


parenthesise([],_,_,[]) :-
	!.
parenthesise([N|Ns],G,S,[[N|Group]]) :-
	backlinked(N,G,Ns),
	!,
	parenthesise(Ns,G,[N|S],Group).
parenthesise([N|Ns],G,S,[N|Group]) :-
	parenthesise(Ns,G,[N|S],Group).

backlinked(N,G,S) :-
	search_tree(G,N,links(_,Bs)),
	setintersect(Bs,S,[_|_]).


	
writeWto(Wto,S) :-
	write(S,Wto),nl(S).
writeWPs(Wto,S) :-
	write(S,Wto),nl(S).
