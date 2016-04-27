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
id___0(A,B) :-
   id__1(B),
   B=0,
   A=0.
id__3(A) :-
   id__1(A),
   A<0.
id__3(A) :-
   id__1(A),
   A>0.
id___0(A,B) :-
   id__3(B),
   id(1,0,0,B+ -1,C),
   A=C+1.
id__split(A,B) :-
   id___0(A,B).
id(A,B,C,D,E) :-
   A=1,
   B=0,
   C=0,
   id__split(E,D).
main_entry :-
   true.
main__un :-
   main_entry,
   id(1,0,0,5,A),
   A=5.
main_verifier_error :-
   main__un.

