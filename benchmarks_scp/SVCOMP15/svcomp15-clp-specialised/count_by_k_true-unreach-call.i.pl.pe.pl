false :-
   main_verifier_error.
main_entry :-
   true.
main__un(A,B) :-
   -1*B> -11,
   1*A=0,
   main_entry.
main__un1(A,B) :-
   -1*B> -11,
   -1*A+1000000*B>0,
   1*A>=0,
   main__un(A,B).
main_orig_main_exit(A,B) :-
   1*A+ -1000000*B>=0,
   -1*B> -11,
   1*A>=0,
   main__un(A,B).
main__un(A,B) :-
   1*A+ -1*B>=0,
   -1*B> -11,
   -1*A+1000001*B>0,
   1*A+ -1*B+ -1*C=0,
   main__un1(C,B).
main_precall(A) :-
   1*B+ -1000000*C>0,
   -1*C> -11,
   1*B>=0,
   1*A=0,
   1*D=0,
   main_orig_main_exit(B,C).
main___VERIFIER_assert(A) :-
   1*A=0,
   main_precall(A).
main__un2 :-
   1*A=0,
   main___VERIFIER_assert(A).
main_verifier_error :-
   main__un2.

