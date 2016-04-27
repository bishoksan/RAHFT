false :-
   main_verifier_error.
main_entry :-
   true.
main__un(A,B) :-
   1*A=0,
   1*B=1,
   main_entry.
main__un1(A,B) :-
   -1*A+1*B>=1,
   -1*A> -6,
   -2*A+1*B>=0,
   1*A>=0,
   main__un(A,B).
main_orig_main_exit(A) :-
   1*B>=6,
   -1*B> -7,
   1*A+ -2*B>=0,
   main__un(B,A).
main__un(A,B) :-
   -4*A+1*B>= -4,
   -1*A> -7,
   -2*A+1*B>=0,
   1*A>=1,
   1*A+ -1*C=1,
   1*B+ -2*D=0,
   main__un1(C,D).
main_precall(A) :-
   1*A=0,
   1*B=64,
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

