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
   A=9,
   B= -100,
   C=0.
main__un1(A,B) :-
   main__un(C,A,B),
   B<101.
main_orig_main_exit(A) :-
   main__un(A,B,C),
   C>=101.
main__un2(A,B) :-
   main__un1(B,C),
   A=C+1.
main__un3(A,B) :-
   main__un2(A,B),
   B<20.
main__un4(A,B,C) :-
   main__un2(A,B),
   B>=20,
   C=4.
main__un2(A,B) :-
   main__un3(A,C),
   B=A+C.
main__un5(A,B,C) :-
   main__un4(A,B,C),
   C<4.
main__un(A,B,C) :-
   main__un4(C,B,A),
   A>=4.
main__un4(A,B,C) :-
   main__un5(A,B,D),
   C=D+1.
main_precall(A) :-
   main_orig_main_exit(B),
   B=4,
   A=1,
   C=0.
main_precall(A) :-
   main_orig_main_exit(B),
   B<4,
   A=0,
   C=0.
main_precall(A) :-
   main_orig_main_exit(B),
   B>4,
   A=0,
   C=0.
main___VERIFIER_assert(A) :-
   main_precall(A).
main__un6 :-
   A=0,
   main___VERIFIER_assert(A).
main_verifier_error :-
   main__un6.

