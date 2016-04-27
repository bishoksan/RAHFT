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
main__un(A,B,C,D) :-
   main_entry,
   D+ -10<9991,
   C+ -10<9991,
   A=0,
   B=0.
main__un1(A,B,C,D,E) :-
   main__un(C,A,D,E),
   C<E,
   B=0.
main_orig_main_exit(A) :-
   main__un(B,A,C,D),
   B>=D.
main__un2(A,B,C,D,E) :-
   main__un1(A,B,C,D,E),
   B<D.
main__un3(A,B,C,D) :-
   main__un1(A,E,B,C,D),
   E>=C.
main__un1(A,B,C,D,E) :-
   main__un2(F,G,C,D,E),
   A=F+1,
   B=G+1.
main__un(A,B,C,D) :-
   main__un3(B,E,C,D),
   A=E+1.
main_precall(A) :-
   main_orig_main_exit(B),
   B>99,
   A=1,
   C=0.
main_precall(A) :-
   main_orig_main_exit(B),
   B=<99,
   A=0,
   C=0.
main___VERIFIER_assert(A) :-
   main_precall(A).
main__un4 :-
   A=0,
   main___VERIFIER_assert(A).
main_verifier_error :-
   main__un4.

