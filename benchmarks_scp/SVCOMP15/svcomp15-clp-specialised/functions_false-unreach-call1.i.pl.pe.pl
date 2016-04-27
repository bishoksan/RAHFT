false :-
   main_verifier_error.
f__1(A) :-
   1*A>=0.
f__split(A,B) :-
   -1*A> -268435457,
   1*A>=2,
   1*A+ -1*B=2,
   f__1(B).
f(A,B,C,D,E) :-
   -1*D> -268435455,
   1*D>=0,
   1*A=1,
   1*B=0,
   1*C=0,
   1*D+ -1*E= -2,
   f__split(E,D).
main_entry :-
   true.
main__un(A) :-
   1*A=0,
   main_entry.
main__un1(A) :-
   -1*A> -268435455,
   1*A>=0,
   main__un(A).
main_orig_main_exit :-
   -1*A> -268435457,
   1*A>=268435455,
   main__un(A).
main__un(A) :-
   -1*A> -268435457,
   1*A>=2,
   1*B=1,
   1*C=0,
   1*D=0,
   1*A+ -1*E=2,
   main__un1(E),
   f(B,C,D,E,A).
main_precall(A) :-
   1*B=0,
   main_orig_main_exit.
main___VERIFIER_assert(A) :-
   main_precall(A).
main__un2 :-
   1*A=0,
   main___VERIFIER_assert(A).
main_verifier_error :-
   main__un2.

