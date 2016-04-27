false :-
   main_verifier_error.
main_entry :-
   true.
main__un(A,B) :-
   1*A=0,
   1*B=0,
   main_entry.
main_orig_main_exit(A) :-
   1*C>=0,
   -1*C> -1,
   1*A>=50,
   1*B= -1,
   main__un(A,C).
main__un1(A,B) :-
   1*B>=1,
   -1*A> -50,
   1*A+ -1*B>= -1,
   1*C=1,
   1*B+ -1*D=1,
   main__un(A,D).
main__un1(A,B) :-
   1*B>=0,
   -1*B> -50,
   1*A+ -1*B>=1,
   1*A>=50,
   1*C= -1,
   1*B+ -1*D= -1,
   main__un(A,D).
main__un(A,B) :-
   1*B>=0,
   1*A+ -1*B>=0,
   -1*B> -51,
   1*A+50*B>=51,
   1*A+ -1*C=1,
   main__un1(C,B).
main_precall(A) :-
   -1*B> -100,
   1*B>=50,
   1*A=0,
   1*C=0,
   main_orig_main_exit(B).
main_precall(A) :-
   1*B>100,
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

