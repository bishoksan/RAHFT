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
   B+ -1<1000,
   A=0,
   C=1.
main_orig_main_exit(A,B) :-
   main__un(A,B,C),
   C>B.
main__un1(A,B,C) :-
   main__un(A,B,C),
   C=<B.
main__un(A,B,C) :-
   main__un1(D,B,E),
   A=D+E,
   C=E+1.
main_precall(A) :-
   main_orig_main_exit(B,C),
   B*2=C*(C+1),
   A=1,
   D=0.
main_precall(A) :-
   main_orig_main_exit(B,C),
   B*2<C*(C+1),
   A=0,
   D=0.
main_precall(A) :-
   main_orig_main_exit(B,C),
   B*2>C*(C+1),
   A=0,
   D=0.
main___VERIFIER_assert(A) :-
   main_precall(A).
main__un2 :-
   A=0,
   main___VERIFIER_assert(A).
main_verifier_error :-
   main__un2.

