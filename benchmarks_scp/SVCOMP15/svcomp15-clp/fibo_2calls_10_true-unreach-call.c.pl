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
fibo2(A,B,C,D,E) :-
   A=1,
   B=1,
   C=1.
fibo2(A,B,C,D,E) :-
   A=0,
   B=1,
   C=1.
fibo2(A,B,C,D,E) :-
   A=0,
   B=0,
   C=0.
fibo2__1(A) :-
   true.
fibo2___0(A,B) :-
   fibo2__1(B),
   B<1,
   A=0.
fibo2__3(A) :-
   fibo2__1(A),
   A>=1.
fibo2___0(A,B) :-
   fibo2__3(B),
   B=1,
   A=1.
fibo2__5(A) :-
   fibo2__3(A),
   A<1.
fibo2__5(A) :-
   fibo2__3(A),
   A>1.
fibo2___0(A,B) :-
   fibo2__5(B),
   A=C+D.
fibo2__split(A,B) :-
   fibo2___0(A,B).
fibo2(A,B,C,D,E) :-
   A=1,
   B=0,
   C=0,
   fibo2__split(E,D).
main_entry :-
   true.
main__un :-
   main_entry,
   A<55.
main__un :-
   main_entry,
   A>55.
main_verifier_error :-
   main__un.

