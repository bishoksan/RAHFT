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
main__un(A) :-
   1*A=0,
   main_entry.
main__un1(A) :-
   -1*A> -1000000,
   1*A>=0,
   main__un(A).
main__un1(A) :-
   1*A>1000000,
   main__un(A).
main_postcall(A,B) :-
   -1*B> -1000001,
   1*B>=0,
   1*A=1,
   1*C=1,
   main__un1(B).
main_postcall(A,B) :-
   1*B>=1000001,
   1*A=0,
   1*C=1,
   main__un1(B).
main_precall(A) :-
   1*B>=1000001,
   1*A=0,
   1*C=0,
   main__un1(B).
main___VERIFIER_assert(A) :-
   1*A=0,
   main_precall(A).
main__un(A) :-
   1*F>0,
   -1*F>= -1,
   1*A+1000001*F>=1000002,
   1*B=1,
   1*C=0,
   1*D=0,
   1*A+ -1*E=1,
   main_postcall(F,E),
   p____VERIFIER_assert(B,C,D,F).
main__un2 :-
   1*A=0,
   main___VERIFIER_assert(A).
main_verifier_error :-
   main__un2.

