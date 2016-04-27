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
fibonacci(A,B,C,D,E) :-
   A=1,
   B=1,
   C=1.
fibonacci(A,B,C,D,E) :-
   A=0,
   B=1,
   C=1.
fibonacci(A,B,C,D,E) :-
   A=0,
   B=0,
   C=0.
fibonacci__1(A) :-
   true.
fibonacci___0(A,B) :-
   fibonacci__1(B),
   B<1,
   A=0.
fibonacci__3(A) :-
   fibonacci__1(A),
   A>=1.
fibonacci___0(A,B) :-
   fibonacci__3(B),
   B=1,
   A=1.
fibonacci__5(A) :-
   fibonacci__3(A),
   A<1.
fibonacci__5(A) :-
   fibonacci__3(A),
   A>1.
fibonacci___0(A,B) :-
   fibonacci__5(B),
   fibonacci(1,0,0,B+ -1,C),
   fibonacci(1,0,0,B+ -2,D),
   A=C+D.
fibonacci__split(A,B) :-
   fibonacci___0(A,B).
fibonacci(A,B,C,D,E) :-
   A=1,
   B=0,
   C=0,
   fibonacci__split(E,D).
main_entry :-
   true.
main__un :-
   main_entry,
   fibonacci(1,0,0,9,A),
   A<34.
main__un :-
   main_entry,
   fibonacci(1,0,0,9,A),
   A>34.
main_verifier_error :-
   main__un.

