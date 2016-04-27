main_entry :-
   true.
main__un(A,B) :-
   1*A=0,
   main_entry.
main__un1(A,B) :-
   -1*A+1*B>0,
   1*A>=0,
   main__un(A,B).
main_orig_main_exit :-
   1*A+ -1*B>=0,
   1*A>=0,
   main__un(A,B).
main__un(A,B) :-
   1*A>=2,
   -1*A+1*B> -2,
   1*A+ -1*C=2,
   main__un1(C,B).
main_precall :-
   1*A=0,
   main_orig_main_exit.
main___VERIFIER_assert :-
   main_precall.

