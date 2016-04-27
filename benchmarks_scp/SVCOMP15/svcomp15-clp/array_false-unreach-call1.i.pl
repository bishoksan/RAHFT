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
   A=0.
main__un1(A) :-
   main__un(A),
   A<1024.
main_orig_main_exit :-
   main__un(A),
   A>=1024.
main__un(A) :-
   main__un1(B),
   A=B+1.
main_precall(A) :-
   main_orig_main_exit,
   B<1023,
   A=1,
   C=0.
main_precall(A) :-
   main_orig_main_exit,
   B>1023,
   A=1,
   C=0.
main_precall(A) :-
   main_orig_main_exit,
   B=1023,
   A=0,
   C=0.
main___VERIFIER_assert(A) :-
   main_precall(A).
main__un2 :-
   A=0,
   main___VERIFIER_assert(A).
main_verifier_error :-
   main__un2.

