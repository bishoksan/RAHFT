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
   B=50.
main__un1(A,B) :-
   main__un(A,B),
   A<100.
main_orig_main_exit(A) :-
   main__un(B,A),
   B>=100.
main__un(A,B) :-
   main__un1(C,D),
   C>49,
   E=1,
   A=C+1,
   B=D+E.
main__un(A,B) :-
   main__un1(C,D),
   C=<49,
   E=0,
   A=C+1,
   B=D+E.
main_precall(A) :-
   main_orig_main_exit(B),
   B=100,
   A=1,
   C=0.
main_precall(A) :-
   main_orig_main_exit(B),
   B<100,
   A=0,
   C=0.
main_precall(A) :-
   main_orig_main_exit(B),
   B>100,
   A=0,
   C=0.
main___VERIFIER_assert(A) :-
   main_precall(A).
main__un2 :-
   A=0,
   main___VERIFIER_assert(A).
main_verifier_error :-
   main__un2.

