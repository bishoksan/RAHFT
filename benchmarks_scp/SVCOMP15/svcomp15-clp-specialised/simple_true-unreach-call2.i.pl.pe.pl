main_entry :-
   true.
main__un(A) :-
   main_entry.
main__un1(A) :-
   -1*A> -268435455,
   main__un(A).
main_orig_main_exit(A) :-
   1*A>=268435455,
   main__un(A).
main__un(A) :-
   -1*A> -268435456,
   1*A+ -1*B=1,
   main__un1(B).

