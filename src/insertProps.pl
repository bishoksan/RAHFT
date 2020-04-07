:- module(insertProps, [main/1], [dynamic]).

% Input: a program P, a file F containing an interpretation of P
% Output: a program P1
%
% For each clause H :- C, B1, ..., Bn in P, it produces a new clause H
% :- C0,C, C1,..., Cn, B1, ..., Bn where C0, Ci are interpreation of the
% predicates H and Bi. For further details: Section 4.3 of
% http://akira.ruc.dk/~kafle/publications/pepm-15

:- use_module(library(streams)).
:- use_module(library(write)).
:- use_module(library(read)).
:- use_module(library(terms_vars)).
:- use_module(library(ppl)).
:- use_module(library(lists)).

:- use_module(chclibs(setops)).
:- use_module(chclibs(canonical)).
:- use_module(chclibs(linearize)).
:- use_module(chclibs(timer_ciao)).
:- use_module(chclibs(program_loader)).
:- use_module(chclibs(ppl_ops)).
:- use_module(chclibs(common)).

:- include(chclibs(get_options)).
:- include(chclibs(messages)).

:- dynamic(fact/2).
:- dynamic(prop/2).
:- dynamic(pe_clause/2).
:- data opt_array/0.
:- data flag/1. % TODO: use

recognised_option('-v', verbose, []).
recognised_option('-prg',   programO(R),[R]).
recognised_option('-props', propFile(R),[R]).
recognised_option('-array', array,[]).
recognised_option('-o',     outputFile(R),[R]).

main(ArgV) :-
    cleanup,
    get_options(ArgV,Options,_),
    setOptions(Options,File,OutS),
    verbose_message(['Starting insertProps...']),
    load_file(File),
    %start_time,
    start_ppl,
    operator,
    %end_time(user_output),
    writeClauses(OutS),
    nl(OutS),
    close(OutS),
    ppl_finalize.

    
setOptions(Options,File,OutS) :-
    ( member(programO(File),Options) -> true
    ; write(user_output,'No input file given.'),nl(user_output),fail
    ),
    ( member(outputFile(OutFile),Options) ->
        open(OutFile,write,OutS)
    ; OutS=user_output
    ),
    ( member(propFile(PFile),Options) ->
        readPropFile(PFile)
    ; true
    ),
    retractall_fact(flag(verbose)),
    ( member(verbose, Options) ->
        assertz_fact(flag(verbose))
    ; true
    ),
    ( member(array,Options) ->
        assertz_fact(opt_array)
    ; true
    ).

cleanup :-
    retractall(fact(_,_)),
    retractall(prop(_,_)),
    retractall(my_clause(_,_,_)),
    retractall(pe_clause(_,_)).
    
operator:-
    my_clause(Head,B,_),
    separate_constraints(B,Cs,Bs),
    answerConstraint(Head,Cs1),
    %findall(prop(Head,Cs1),prop(Head,Cs1),Props),
    %Props = [_,_], % exists both an answer and a query 
    %appendConstraints(Props,Head,Cs,Cs2),
    append(Cs1,Cs,Cs2),
    bodyAnswerConstraints(Bs,Cs3),
    append(Cs2,Cs3,Cs4),
    ( opt_array ->
    numbervars((Head,Cs4,Bs),0,_),
        % DONE:{arrays(3)}
        %   let H:- C_l, C_a, B be a clause with C_l being linear arithmetic
        %   constraint and C_a being array constraints. Say we need to strengthen
        %   it with lets say C. Then the resulting clause is simply H:- C, C_l,
        %   C_a, B. The clause is useless if C, C_l, C_a is
        %   unsatisfiable. Therefore satisfiability is checked in
        %   insertProps.pl. Here we can do several things:
        %   
        %   (1) leave as it is H:- C, C_l, C_a, B
        %   (2) check sat of C, C_l using linear solver; if unsat remove the clause
        %   (3) check sat of C, C_l, C_a using smt solvers.
        write(user_output, 'TODO: check sat of C,C_l,C_a'),
        separate_array_constraints(Cs4, Cs4a, Cs4r),
        display(satisfiable(Cs4,H)), nl,
        display(array(Cs4a,Cs4r)), nl,
        % TODO:{arrays} implement option 2 (use yices for array constraints)
        satisfiable(Cs4r,H),
    getConstraint(H,Cs5),
        display(was_satisfiable), nl
    ;
    separateLinearConstraints(Cs4,CsLin,CsN),
    numbervars((Head,CsLin,Bs,CsN),0,_),
    satisfiable(CsLin,H),
    getConstraint(H,Cs6),
    append(Cs6,CsN,Cs5)
    ),
    append(Cs5,Bs,B1),
    assertz(pe_clause(Head,B1)),
    fail.
operator.

bodyAnswerConstraints([],[]).
bodyAnswerConstraints([B|Bs],Cs) :-
    bodyAnswerConstraints(Bs,Cs1),
    answerConstraint(B,Cs2),
    append(Cs2,Cs1,Cs).
    
answerConstraint(A,Cs) :-
    A =.. [P|Xs],
    name(P,PName),
    append(PName,"_ans",AName),  % first look for the _ans predicate
    name(P_ans,AName),
    A_ans =.. [P_ans|Xs],
    prop(A_ans,Cs),
    !.
answerConstraint(A,Cs) :-        % otherwise just get the original predicate
    prop(A,Cs).
    
appendConstraints([],_,Cs,Cs).
appendConstraints([prop(Head,Cs1)|Props],Head,Cs,Cs3) :-
    appendConstraints(Props,Head,Cs,Cs2),
    append(Cs1,Cs2,Cs3).

separateLinearConstraints([],[],[]).
separateLinearConstraints([C|Cs],[C|Cs1],Cs2) :-
    linear_constraint(C),
    !,
    separateLinearConstraints(Cs,Cs1,Cs2).
separateLinearConstraints([C|Cs],Cs1,[C|Cs2]) :-
    separateLinearConstraints(Cs,Cs1,Cs2).

% TODO: unused
%%unsat(Cs) :-
%%      linearize(Cs,Cs1),
%%      numbervars(Cs1,0,_),
%%      \+ satisfiable(Cs1,_).
    
% TODO: unused
%%solve(Xs,Cs,Hp) :-
%%      linearize(Cs,Cs1),
%%      varset((Xs,Cs1),Ys),
%%      dummyCList(Ys,DCL),
%%      append(Cs1,DCL,CsL),
%%      numbervars((Xs:-CsL),0,_),
%%      satisfiable(CsL,H1),
%%      setdiff(Ys,Xs,Zs),
%%      project(H1,Zs,Hp).

       
record(Head,H):-
    cond_assert(Head,H).
    
cond_assert(Head,H):-
    \+ alreadyAsserted(Head,H),
    assertz(fact(Head,H)).
            
alreadyAsserted(Head,H) :-
    fact(Head,H1), 
    contains(H,H1),
    contains(H1,H).
    

readPropFile(PFile) :-
    open(PFile,read,S),
    read(S,C),
    readPropFacts(S,C),
    close(S).
    
readPropFacts(_,end_of_file) :-
    !.
readPropFacts(S,(H:-C)) :-
    varset(H,Xs),
    dummyCList(Xs,DCL),
    append(C,DCL,CsL),
    assertz(prop(H,CsL)),
    read(S,C1),
    readPropFacts(S,C1).

writeClauses(S) :-
    pe_clause(H,B),
    writeq(S,H),
    write(S,' :-'),
    nl(S),
    writeBodyAtoms(S,B),
    write(S,'.'),
    nl(S),
    fail.
writeClauses(_).
    
writeBodyAtoms(S,[]) :-
    !,
    write(S,'   '),
    write(S,true).
writeBodyAtoms(S,[B]) :-
    !,
    write(S,'   '),
    writeq(S,B).
writeBodyAtoms(S,[B1,B2|Bs]) :-
    write(S,'   '),
    writeq(S,B1),
    write(S,','),
    nl(S),
    writeBodyAtoms(S,[B2|Bs]).
