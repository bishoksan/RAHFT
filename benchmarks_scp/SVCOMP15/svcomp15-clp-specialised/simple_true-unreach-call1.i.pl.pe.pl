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
   1*A+ -1*B=2,
   main__un1(B).
main_precall :-
   1*A=0,
   main_orig_main_exit.
main___VERIFIER_assert :-
   main_precall.

