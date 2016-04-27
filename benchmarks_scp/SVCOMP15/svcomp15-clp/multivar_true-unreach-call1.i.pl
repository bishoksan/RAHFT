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
   B=A,
   main_entry.
main__un1(A,B) :-
   main__un(A,B),
   A<1024.
main_orig_main_exit(A,B) :-
   main__un(A,B),
   A>=1024.
main__un(A,B) :-
   main__un1(C,D),
   A=C+1,
   B=D+1.
main_precall(A) :-
   main_orig_main_exit(B,C),
   B=C,
   A=1,
   D=0.
main_precall(A) :-
   main_orig_main_exit(B,C),
   B<C,
   A=0,
   D=0.
main_precall(A) :-
   main_orig_main_exit(B,C),
   B>C,
   A=0,
   D=0.
main___VERIFIER_assert(A) :-
   main_precall(A).
main__un2 :-
   A=0,
   main___VERIFIER_assert(A).
main_verifier_error :-
   main__un2.

