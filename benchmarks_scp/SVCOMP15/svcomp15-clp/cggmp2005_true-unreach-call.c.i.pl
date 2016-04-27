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
main__un(A,B) :-
   main_entry,
   A=10,
   B=1.
main_orig_main_exit(A) :-
   main__un(A,B),
   A<B.
main__un1(A,B) :-
   main__un(A,B),
   A>=B.
main__un(A,B) :-
   main__un1(C,D),
   A=C+ -1,
   B=D+2.
main_precall(A) :-
   main_orig_main_exit(B),
   B=6,
   A=1,
   C=0.
main_precall(A) :-
   main_orig_main_exit(B),
   B<6,
   A=0,
   C=0.
main_precall(A) :-
   main_orig_main_exit(B),
   B>6,
   A=0,
   C=0.
main___VERIFIER_assert(A) :-
   main_precall(A).
main__un2 :-
   A=0,
   main___VERIFIER_assert(A).
main_verifier_error :-
   main__un2.

