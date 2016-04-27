false :-
   main_verifier_error.
main_entry :-
   true.
main__un(A,B,C,D) :-
   -1*D> -10001,
   -1*C> -10001,
   1*A=0,
   1*B=0,
   main_entry.
main__un1(A,B,C,D,E) :-
   1*C>=0,
   -1*E> -10001,
   -1*D> -10001,
   -1*C+1*E>0,
   1*A>=0,
   1*B=0,
   main__un(C,A,D,E).
main_orig_main_exit(A) :-
   1*B>=0,
   1*B+ -1*C>=0,
   -1*D> -10001,
   -1*C> -10001,
   1*A>=0,
   main__un(B,A,D,C).
main__un2(A,B,C,D,E) :-
   1*B>=0,
   1*C>=0,
   -1*E> -10001,
   -1*D> -10001,
   -1*C+1*E>0,
   -1*B+1*D>0,
   1*A+ -1*B>=0,
   main__un1(A,B,C,D,E).
main__un3(A,B,C,D) :-
   1*B>=0,
   1*E>=0,
   -1*D> -10001,
   -1*C+1*E>=0,
   -1*C> -10001,
   -1*B+1*D>0,
   1*A+ -1*E>=0,
   main__un1(A,E,B,C,D).
main__un1(A,B,C,D,E) :-
   -1*B+1*D> -1,
   1*A+ -1*B>=0,
   1*B>=1,
   -1*E> -10001,
   -1*D> -10001,
   -1*C+1*E>0,
   1*C>=0,
   1*A+ -1*F=1,
   1*B+ -1*G=1,
   main__un2(F,G,C,D,E).
main__un(A,B,C,D) :-
   1*B+ -1*C>=0,
   -1*A+1*D> -1,
   1*A>=1,
   -1*D> -10001,
   -1*C> -10001,
   1*B>=0,
   1*A+ -1*E=1,
   main__un3(B,E,C,D).
main_precall(A) :-
   -1*B>= -99,
   1*B>=0,
   1*A=0,
   1*C=0,
   main_orig_main_exit(B).
main___VERIFIER_assert(A) :-
   1*A=0,
   main_precall(A).
main__un4 :-
   1*A=0,
   main___VERIFIER_assert(A).
main_verifier_error :-
   main__un4.

