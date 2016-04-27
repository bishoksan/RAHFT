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
id2___0(A,B) :-
   id2__1(B),
   B=0,
   A=0.
id2__3(A) :-
   id2__1(A),
   A<0.
id2__3(A) :-
   id2__1(A),
   A>0.
id2___0(A,B) :-
   id2__3(B),
   A=C+1.
id2__split(A,B) :-
   id2___0(A,B).
id2(A,B,C,D,E) :-
   A=1,
   B=0,
   C=0,
   id2__split(E,D).
main_entry :-
   true.
main__un :-
   main_entry,
   A<5.
main__un :-
   main_entry,
   A>5.
main_verifier_error :-
   main__un.

