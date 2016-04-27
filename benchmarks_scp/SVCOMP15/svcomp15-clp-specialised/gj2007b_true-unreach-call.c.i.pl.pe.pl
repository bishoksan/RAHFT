false :-
   main_verifier_error.
p____VERIFIER_assert__1(A) :-
   true.
p____VERIFIER_assert__ret(A) :-
   -1*A>0,
   p____VERIFIER_assert__1(A).
p____VERIFIER_assert__ret(A) :-
   1*A>0,
   p____VERIFIER_assert__1(A).
p____VERIFIER_assert(A,B,C,D) :-
   1*A=1,
   1*B=0,
   1*C=0,
   p____VERIFIER_assert__ret(D).
main_entry :-
   true.
main__un(A,B,C) :-
   1*A=0,
   1*B=0,
   main_entry.
main__un1(A,B,C) :-
   1*B>=0,
   -1*B+1*C>0,
   1*A>=0,
   main__un(A,B,C).
main_orig_main_exit(A,B) :-
   1*C>=0,
   -1*B+1*C>=0,
   1*A>=0,
   main__un(A,C,B).
main__un(A,B,C) :-
   1*A>=0,
   1*B>=1,
   -1*B+1*C> -1,
   1*D=0,
   1*A+ -1*E=0,
   1*B+ -1*F=1,
   main__un1(E,F,C).
main__un(A,B,C) :-
   1*A>=0,
   1*F>=0,
   -1*D>0,
   -1*A+1*C>0,
   1*A+ -1*B= -1,
   1*A+ -1*E=0,
   main__un1(F,E,C).
main__un(A,B,C) :-
   1*D>0,
   1*A>=0,
   1*F>=0,
   -1*A+1*C>0,
   1*A+ -1*B= -1,
   1*A+ -1*E=0,
   main__un1(F,E,C).
main_postcall(A,B,C) :-
   -1*C> -1,
   1*A>=0,
   1*D=1,
   1*E=1,
   main_orig_main_exit(A,C).
main_postcall(A,B,C) :-
   1*C>=1,
   1*A>=0,
   1*D=1,
   1*E=0,
   main_orig_main_exit(A,C).
main_precall(A) :-
   -1*B> -1,
   1*E>=0,
   1*C=1,
   1*D=0,
   main_orig_main_exit(E,B).
main_precall(A) :-
   1*E>=0,
   1*B>=1,
   1*C=0,
   1*D=0,
   main_orig_main_exit(E,B).
main___VERIFIER_assert(A) :-
   main_precall(A).
main_precall2(A) :-
   -1*F> -1,
   1*G>=0,
   1*A=1,
   1*B=1,
   1*C=0,
   1*D=0,
   1*E=0,
   main_postcall(G,H,F),
   p____VERIFIER_assert(B,C,D,H).
main_precall2(A) :-
   -1*F+1*G>0,
   1*F>=0,
   1*A=1,
   1*B=1,
   1*C=0,
   1*D=0,
   1*E=0,
   main_postcall(F,H,G),
   p____VERIFIER_assert(B,C,D,H).
main_precall2(A) :-
   -1*F+1*G>=0,
   1*F>=1,
   1*A=0,
   1*B=1,
   1*C=0,
   1*D=0,
   1*E=0,
   main_postcall(G,H,F),
   p____VERIFIER_assert(B,C,D,H).
main___VERIFIER_assert(A) :-
   -1*A>= -1,
   1*A>=0,
   main_precall2(A).
main__un2 :-
   1*A=0,
   main___VERIFIER_assert(A).
main_verifier_error :-
   main__un2.

