false :-
   main_verifier_error.
main_entry :-
   true.
main__un(A,B) :-
   1*A=10,
   1*B=1,
   main_entry.
main_orig_main_exit(A) :-
   -1*A> -7,
   1*A>=6,
   2*A+1*B=21,
   main__un(A,B).
main__un1(A,B) :-
   -1*A>= -10,
   1*A>=7,
   2*A+1*B=21,
   main__un(A,B).
main__un(A,B) :-
   -1*A>= -9,
   1*A>=6,
   2*A+1*B=21,
   1*A+ -1*C= -1,
   2*A+1*D=19,
   main__un1(C,D).
main_precall(A) :-
   -1*B> -7,
   1*B>6,
   1*A=0,
   1*C=0,
   main_orig_main_exit(B).
main___VERIFIER_assert(A) :-
   1*A=0,
   main_precall(A).
main__un2 :-
   1*A=0,
   main___VERIFIER_assert(A).
main_verifier_error :-
   main__un2.

