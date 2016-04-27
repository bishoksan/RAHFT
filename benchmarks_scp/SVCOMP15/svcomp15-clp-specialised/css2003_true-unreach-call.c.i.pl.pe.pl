false :-
   main_verifier_error.
p____VERIFIER_assert__1(A) :-
   -1*A>= -1,
   1*A>=0.
p____VERIFIER_assert__ret(A) :-
   -1*A>= -1,
   1*A>0,
   p____VERIFIER_assert__1(A).
p____VERIFIER_assert(A,B,C,D) :-
   -1*D>= -1,
   1*D>0,
   1*A=1,
   1*B=0,
   1*C=0,
   p____VERIFIER_assert__ret(D).
main_entry :-
   true.
main__un(A,B) :-
   -1*A> -2,
   1*B=1,
   main_entry.
main__un1(A,B) :-
   -1*B> -1000000,
   -1*A+ -1*B> -3,
   1*B>=1,
   main__un(A,B).
main_postcall(A,B,C) :-
   1*C>=2,
   -1*B+ -1*C> -3,
   -1*C> -1000001,
   1*B+1*C>0,
   1*A=1,
   1*D=1,
   1*B+ -1*E= -1,
   1*C+ -1*F=1,
   main__un1(E,F).
main_postcall(A,B,C) :-
   -1*B+ -1*C>=0,
   -1*C> -1000001,
   1*C>=2,
   1*A=0,
   1*D=1,
   1*B+ -1*E= -1,
   1*C+ -1*F=1,
   main__un1(E,F).
main_precall(A) :-
   -1*C> -1000000,
   -1*C+ -1*D>=0,
   1*C>=1,
   1*A=0,
   1*B=0,
   main__un1(D,C).
main___VERIFIER_assert(A) :-
   1*A=0,
   main_precall(A).
main__un(A,B) :-
   1*F>0,
   -1*F>= -1,
   -1*B> -1000001,
   -1*A+ -1*B+3*F>=0,
   -1*A+ -1*B+2*F> -1,
   1*B>=2,
   1*C=1,
   1*D=0,
   1*E=0,
   main_postcall(F,A,B),
   p____VERIFIER_assert(C,D,E,F).
main__un2 :-
   1*A=0,
   main___VERIFIER_assert(A).
main_verifier_error :-
   main__un2.

