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
id(A,B,C,D,E) :-
   A=1,
   B=1,
   C=1.
id(A,B,C,D,E) :-
   A=0,
   B=1,
   C=1.
id(A,B,C,D,E) :-
   A=0,
   B=0,
   C=0.
id__1(A) :-
   true.
id__br1(A) :-
   id__1(A),
   A=0.
id__3(A) :-
   id__1(A),
   A<0.
id__3(A) :-
   id__1(A),
   A>0.
id_UnifiedReturnBlock(A,B) :-
   id__3(B),
   id(1,0,0,B+ -1,C),
   C>1,
   A=2.
id_UnifiedReturnBlock(A,B) :-
   id__3(B),
   id(1,0,0,B+ -1,C),
   C=<1,
   A=C+1.
id_UnifiedReturnBlock(A,B) :-
   id__br1(B),
   A=0.
id_UnifiedReturnBlock_split(A,B) :-
   id_UnifiedReturnBlock(A,B).
id(A,B,C,D,E) :-
   A=1,
   B=0,
   C=0,
   id_UnifiedReturnBlock_split(E,D).
main_entry :-
   true.
main__un :-
   main_entry,
   id(1,0,0,A,B),
   B=3.
main_verifier_error :-
   main__un.

