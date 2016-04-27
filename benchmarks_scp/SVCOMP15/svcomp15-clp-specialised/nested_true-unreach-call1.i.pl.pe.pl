false :-
   main_verifier_error.
main_entry :-
   true.
main__un(A) :-
   1*A=0,
   main_entry.
main__un1(A,B) :-
   -1*B> -268435455,
   1*B>=0,
   1*A=0,
   main__un(B).
main_orig_main_exit :-
   1*A>=268435455,
   main__un(A).
main__un2(A,B) :-
   -1*B> -268435455,
   1*B>=0,
   -1*A> -10,
   1*A>=0,
   main__un1(A,B).
main__un3(A) :-
   1*B>=10,
   -1*B> -11,
   -1*A> -268435455,
   1*A>=0,
   main__un1(B,A).
main__un1(A,B) :-
   1*A>=1,
   -1*B> -268435455,
   -1*A> -11,
   1*B>=0,
   1*A+ -1*C=1,
   main__un2(C,B).
main__un(A) :-
   -1*A> -268435456,
   1*A>=1,
   1*A+ -1*B=1,
   main__un3(B).
main_precall(A) :-
   1*B=0,
   main_orig_main_exit.
main___VERIFIER_assert(A) :-
   main_precall(A).
main__un4 :-
   1*A=0,
   main___VERIFIER_assert(A).
main_verifier_error :-
   main__un4.

