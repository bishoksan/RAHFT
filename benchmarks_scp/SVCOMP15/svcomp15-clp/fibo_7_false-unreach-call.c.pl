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
fibo(A,B,C,D,E) :-
   A=1,
   B=1,
   C=1.
fibo(A,B,C,D,E) :-
   A=0,
   B=1,
   C=1.
fibo(A,B,C,D,E) :-
   A=0,
   B=0,
   C=0.
fibo__1(A) :-
   true.
fibo___0(A,B) :-
   fibo__1(B),
   B<1,
   A=0.
fibo__3(A) :-
   fibo__1(A),
   A>=1.
fibo___0(A,B) :-
   fibo__3(B),
   B=1,
   A=1.
fibo__5(A) :-
   fibo__3(A),
   A<1.
fibo__5(A) :-
   fibo__3(A),
   A>1.
fibo___0(A,B) :-
   fibo__5(B),
   fibo(1,0,0,B+ -1,C),
   fibo(1,0,0,B+ -2,D),
   A=C+D.
fibo__split(A,B) :-
   fibo___0(A,B).
fibo(A,B,C,D,E) :-
   A=1,
   B=0,
   C=0,
   fibo__split(E,D).
main_entry :-
   true.
main__un :-
   main_entry,
   fibo(1,0,0,7,A),
   A=13.
main_verifier_error :-
   main__un.

