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
main__un1(A,B) :-
   main__un(B),
   B<268435455,
   A=0.
main_orig_main_exit :-
   main__un(A),
   A>=268435455.
main__un2(A,B) :-
   main__un1(A,B),
   A<10.
main__un3(A) :-
   main__un1(B,A),
   B>=10.
main__un1(A,B) :-
   main__un2(C,B),
   A=C+1.
main__un(A) :-
   main__un3(B),
   A=B+1.
main_precall(A) :-
   main_orig_main_exit,
   B=0.
main___VERIFIER_assert(A) :-
   main_precall(A).
main__un4 :-
   main___VERIFIER_assert(A),
   A=0.
main_verifier_error :-
   main__un4.

