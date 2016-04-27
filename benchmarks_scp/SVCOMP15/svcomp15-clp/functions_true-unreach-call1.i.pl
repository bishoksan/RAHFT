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
f(A,B,C,D,E) :-
   A=1,
   B=1,
   C=1.
f(A,B,C,D,E) :-
   A=0,
   B=1,
   C=1.
f(A,B,C,D,E) :-
   A=0,
   B=0,
   C=0.
f__1(A) :-
   true.
f__split(A,B) :-
   f__1(B),
   A=B+2.
f(A,B,C,D,E) :-
   A=1,
   B=0,
   C=0,
   f__split(E,D).
main_entry :-
   true.
main__un(A) :-
   main_entry,
   A=0.
main__un1(A) :-
   main__un(A),
   A<268435455.
main_orig_main_exit :-
   main__un(A),
   A>=268435455.
main__un(A) :-
   B=1,
   C=0,
   D=0,
   main__un1(E),
   f(B,C,D,E,A).
main_precall(A) :-
   main_orig_main_exit,
   B=0.
main___VERIFIER_assert(A) :-
   main_precall(A).
main__un2 :-
   main___VERIFIER_assert(A),
   A=0.
main_verifier_error :-
   main__un2.

