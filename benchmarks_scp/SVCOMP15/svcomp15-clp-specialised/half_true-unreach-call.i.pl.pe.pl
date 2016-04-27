false :-
   main_verifier_error.
main_entry :-
   true.
main__un(A,B,C) :-
   -1*C> -1000001,
   1*A=0,
   1*B=0,
   main_entry.
main__un1(A,B,C) :-
   -1*C> -1000001,
   -1*B+2*C>0,
   1*B>=0,
   main__un(A,B,C).
main_orig_main_exit(A,B) :-
   -1*B> -1000001,
   -2*B+1*C>=0,
   1*C>=0,
   main__un(A,C,B).
main__un(A,B,C) :-
   1*B>=1,
   -1*C> -1000001,
   -1*B+2*C> -1,
   1*A+ -1*D+ -1*E=0,
   1*B+ -1*F=1,
   main__un1(D,F,C).
main_precall(A) :-
   -1*B> -1000001,
   1*B+ -1*C=0,
   1*D=1,
   1*E=0,
   main_orig_main_exit(B,C).
main_precall(A) :-
   -1*C> -1000001,
   -1*B+1*C>0,
   1*D=0,
   1*E=0,
   main_orig_main_exit(B,C).
main_precall(A) :-
   -1*C> -1000001,
   1*B+ -1*C>0,
   1*D=0,
   1*E=0,
   main_orig_main_exit(B,C).
main___VERIFIER_assert(A) :-
   main_precall(A).
main__un2 :-
   1*A=0,
   main___VERIFIER_assert(A).
main_verifier_error :-
   main__un2.

