false :-
   main_verifier_error.
main_entry :-
   true.
main__un(A) :-
   1*A=0,
   main_entry.
main__un1(A) :-
   -1*A> -6,
   1*A>=0,
   main__un(A).
main_orig_main_exit(A) :-
   -1*A> -7,
   1*A>=6,
   main__un(A).
main__un(A) :-
   -1*A> -7,
   1*A>=1,
   1*A+ -1*B=1,
   main__un1(B).
main_precall(A) :-
   1*A=0,
   1*B=6,
   1*C=0,
   main_orig_main_exit(B).
main___VERIFIER_assert(A) :-
   1*A=0,
   main_precall(A).
main__un2 :-
   1*A=0,
   main___VERIFIER_assert(A).
main_verifier_error :-
   main__un2.

