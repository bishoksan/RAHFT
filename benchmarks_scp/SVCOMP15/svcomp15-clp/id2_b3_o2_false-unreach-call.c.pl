false :-
   main_verifier_error.
verifier_error(A,B,C) :-
   A=0,
   B=0,
   C=0.
verifier_error(A,B,C) :-
   A=0,
   B=1,
   C=1.
verifier_error(A,B,C) :-
   A=1,
   B=0,
   C=1.
verifier_error(A,B,C) :-
   A=1,
   B=1,
   C=1.
id2(A,B,C,D,E) :-
   A=1,
   B=1,
   C=1.
id2(A,B,C,D,E) :-
   A=0,
   B=1,
   C=1.
id2(A,B,C,D,E) :-
   A=0,
   B=0,
   C=0.
id2__1(A) :-
   true.
id2__br1(A) :-
   id2__1(A),
   A=0.
id2__3(A) :-
   id2__1(A),
   A<0.
id2__3(A) :-
   id2__1(A),
   A>0.
id2_UnifiedReturnBlock(A,B) :-
   id2__3(B),
   C>2,
   A=3.
id2_UnifiedReturnBlock(A,B) :-
   id2__3(B),
   C=<2,
   A=C+1.
id2_UnifiedReturnBlock(A,B) :-
   id2__br1(B),
   A=0.
id2_UnifiedReturnBlock_split(A,B) :-
   id2_UnifiedReturnBlock(A,B).
id2(A,B,C,D,E) :-
   A=1,
   B=0,
   C=0,
   id2_UnifiedReturnBlock_split(E,D).
main_entry :-
   true.
main__un :-
   main_entry,
   A=2.
main_verifier_error :-
   main__un.

