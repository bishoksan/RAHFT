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
p____VERIFIER_assert(A,B,C,D) :-
   A=1,
   B=1,
   C=1.
p____VERIFIER_assert(A,B,C,D) :-
   A=0,
   B=1,
   C=1.
p____VERIFIER_assert(A,B,C,D) :-
   A=0,
   B=0,
   C=0.
p____VERIFIER_assert__1(A) :-
   true.
p____VERIFIER_assert__ret(A) :-
   p____VERIFIER_assert__1(A),
   A<0.
p____VERIFIER_assert__ret(A) :-
   p____VERIFIER_assert__1(A),
   A>0.
p____VERIFIER_assert(A,B,C,D) :-
   A=1,
   B=0,
   C=0,
   p____VERIFIER_assert__ret(D).
main_entry :-
   true.
main__un(A) :-
   main_entry,
   A=0.
main__un1(A) :-
   main__un(A),
   A<1000000.
main__un1(A) :-
   main__un(A),
   A>1000000.
main_postcall(A,B) :-
   C=1,
   main__un1(B),
   B<1000001,
   A=1.
main_postcall(A,B) :-
   C=1,
   main__un1(B),
   B>=1000001,
   A=0.
main_precall(A) :-
   main__un1(B),
   B<1000001,
   A=1,
   C=0.
main_precall(A) :-
   main__un1(B),
   B>=1000001,
   A=0,
   C=0.
main___VERIFIER_assert(A) :-
   main_precall(A).
main__un(A) :-
   main_postcall(B,C),
   p____VERIFIER_assert(1,0,0,B),
   A=C+1.
main__un2 :-
   A=0,
   main___VERIFIER_assert(A).
main_verifier_error :-
   main__un2.

