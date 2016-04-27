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
main__un2(A) :-
   main__un(A),
   A<2048.
main_orig_main_exit :-
   main__un(A),
   A>=2048.
main__un(A) :-
   main__un2(B),
   A=B+1.
main_precall(A) :-
   main_orig_main_exit,
   B<C,
   A=1,
   D=0.
main_precall(A) :-
   main_orig_main_exit,
   B>C,
   A=1,
   D=0.
main_precall(A) :-
   main_orig_main_exit,
   B=C,
   A=0,
   D=0.
main___VERIFIER_assert(A) :-
   main_precall(A).
main__un3 :-
   A=0,
   main___VERIFIER_assert(A).
main_verifier_error :-
   main__un3.

