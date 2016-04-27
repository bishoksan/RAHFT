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
main__un1(A) :-
   main__un(A,B),
   A<1024.
main_orig_main_exit(A) :-
   main__un(B,A),
   B>=1024.
main__un(A,B) :-
   main__un1(C),
   A=C+1,
   B=1.
main_precall(A) :-
   main_orig_main_exit(A),
   B=0.
main___VERIFIER_assert(A) :-
   main_precall(A).
main__un2 :-
   main___VERIFIER_assert(A),
   A=0.
main_verifier_error :-
   main__un2.

