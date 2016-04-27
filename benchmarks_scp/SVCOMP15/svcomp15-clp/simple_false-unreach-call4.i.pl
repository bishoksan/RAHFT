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
main_entry :-
   true.
main__un(A) :-
   main_entry,
   A=268435441.
main__un1(A) :-
   main__un(A),
   A>1.
main_orig_main_exit :-
   main__un(A),
   A=<1.
main__un(A) :-
   main__un1(B),
   A=B+ -2.
main_precall(A) :-
   main_orig_main_exit,
   B=0.
main___VERIFIER_assert(A) :-
   main_precall(A).
main__un2 :-
   main___VERIFIER_assert(A),
   A=0.
main_verifier_error :-
   main__un2.

