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
main__un(A,B) :-
   main_entry,
   A<2,
   B=1.
main__un1(A,B) :-
   main__un(A,B),
   B<1000000.
main_postcall(A,B,C) :-
   D=1,
   main__un1(E,F),
   B=E+ -1,
   C=F+1,
   F+E>0,
   F+E<3,
   F> -1,
   A=1.
main_postcall(A,B,C) :-
   D=1,
   main__un1(E,F),
   B=E+ -1,
   C=F+1,
   A=0,
   F=< -1.
main_postcall(A,B,C) :-
   D=1,
   main__un1(E,F),
   B=E+ -1,
   C=F+1,
   A=0,
   F+E=<0.
main_postcall(A,B,C) :-
   D=1,
   main__un1(E,F),
   B=E+ -1,
   C=F+1,
   A=0,
   F+E>=3.
main_precall(A) :-
   main__un1(B,C),
   D=0,
   C+B>0,
   C+B<3,
   C> -1,
   A=1.
main_precall(A) :-
   main__un1(B,C),
   D=0,
   A=0,
   C=< -1.
main_precall(A) :-
   main__un1(B,C),
   D=0,
   A=0,
   C+B=<0.
main_precall(A) :-
   main__un1(B,C),
   D=0,
   A=0,
   C+B>=3.
main___VERIFIER_assert(A) :-
   main_precall(A).
main__un(A,B) :-
   C=1,
   D=0,
   E=0,
   main_postcall(F,A,B),
   p____VERIFIER_assert(C,D,E,F).
main__un2 :-
   A=0,
   main___VERIFIER_assert(A).
main_verifier_error :-
   main__un2.

