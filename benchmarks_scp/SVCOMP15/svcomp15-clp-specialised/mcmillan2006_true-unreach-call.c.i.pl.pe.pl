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
   -1*B> -1001,
   1*A=0,
   main_entry.
main__un1(A,B) :-
   -1*B> -1001,
   -1*A+1*B>0,
   1*A>=0,
   main__un(A,B).
main__un2(A,B) :-
   -1*B+1*C>=0,
   -1*B> -1001,
   1*C>=0,
   1*A=0,
   main__un(C,B).
main__un(A,B) :-
   1*A>=1,
   -1*B> -1001,
   -1*A+1*B> -1,
   1*A+ -1*C=1,
   main__un1(C,B).
main__un3(A,B) :-
   -1*B> -1001,
   -1*A+1*B>0,
   1*A>=0,
   main__un2(A,B).
main_postcall(A,B,C) :-
   -1*C> -1001,
   -1*B+1*C>0,
   1*B>=0,
   1*A=1,
   1*D=1,
   1*E=0,
   main__un3(B,C).
main_postcall(A,B,C) :-
   -1*E>0,
   -1*C> -1001,
   -1*B+1*C>0,
   1*B>=0,
   1*A=0,
   1*D=1,
   main__un3(B,C).
main_postcall(A,B,C) :-
   -1*C> -1001,
   -1*B+1*C>0,
   1*E>0,
   1*B>=0,
   1*A=0,
   1*D=1,
   main__un3(B,C).
main_precall(A) :-
   -1*E> -1001,
   -1*D+1*E>0,
   -1*B>0,
   1*D>=0,
   1*A=0,
   1*C=0,
   main__un3(D,E).
main_precall(A) :-
   1*D>=0,
   -1*E> -1001,
   -1*D+1*E>0,
   1*B>0,
   1*A=0,
   1*C=0,
   main__un3(D,E).
main___VERIFIER_assert(A) :-
   1*A=0,
   main_precall(A).
main__un2(A,B) :-
   1*G>0,
   -1*G>= -1,
   -1*B> -1001,
   1*A>=1,
   -1*A+1*B> -1,
   1*C=1,
   1*D=0,
   1*E=0,
   1*A+ -1*F=1,
   main_postcall(G,F,B),
   p____VERIFIER_assert(C,D,E,G).
main__un4 :-
   1*A=0,
   main___VERIFIER_assert(A).
main_verifier_error :-
   main__un4.

