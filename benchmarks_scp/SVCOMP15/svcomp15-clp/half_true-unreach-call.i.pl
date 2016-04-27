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
main__un(A,B,C) :-
   main_entry,
   C<1000001,
   A=0,
   B=0.
main__un1(A,B,C) :-
   main__un(A,B,C),
   B<C*2.
main_orig_main_exit(A,B) :-
   main__un(A,C,B),
   C>=B*2.
main__un(A,B,C) :-
   main__un1(D,E,C),
   A=D+F,
   B=E+1.
main_precall(A) :-
   main_orig_main_exit(B,C),
   B=C,
   D=1,
   E=0.
main_precall(A) :-
   main_orig_main_exit(B,C),
   B<C,
   D=0,
   E=0.
main_precall(A) :-
   main_orig_main_exit(B,C),
   B>C,
   D=0,
   E=0.
main___VERIFIER_assert(A) :-
   main_precall(A).
main__un2 :-
   main___VERIFIER_assert(A),
   A=0.
main_verifier_error :-
   main__un2.

