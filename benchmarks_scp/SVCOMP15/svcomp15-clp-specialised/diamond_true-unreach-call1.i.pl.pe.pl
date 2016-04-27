false :-
   main_verifier_error.
main_entry :-
   true.
main__un(A) :-
   1*A=0,
   main_entry.
main__un1(A) :-
   -1*A> -99,
   main__un(A).
main_orig_main_exit :-
   1*A>=99,
   main__un(A).
main__un(A) :-
   -1*B> -99,
   1*A+ -1*B+1*C=0,
   main__un1(B).
main_precall(A) :-
   1*B=0,
   main_orig_main_exit.
main___VERIFIER_assert(A) :-
   main_precall(A).
main__un2 :-
   1*A=0,
   main___VERIFIER_assert(A).
main_verifier_error :-
   main__un2.

