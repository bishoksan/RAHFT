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
   A=0,
   B=0.
main__un1(A,B) :-
   main__un(A,B),
   A<1000000.
main_orig_main_exit(A) :-
   main__un(B,A),
   B>=1000000.
main__un(A,B) :-
   main__un1(C,D),
   E+ -1<999999,
   A=C+E,
   B=D+1.
main_precall(A) :-
   main_orig_main_exit(B),
   B<1000001,
   A=1,
   C=0.
main_precall(A) :-
   main_orig_main_exit(B),
   B>=1000001,
   A=0,
   C=0.
main___VERIFIER_assert(A) :-
   main_precall(A).
main__un2 :-
   A=0,
   main___VERIFIER_assert(A).
main_verifier_error :-
   main__un2.

