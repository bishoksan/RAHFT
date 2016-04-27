false :-
   main_verifier_error.
main_entry :-
   true.
main__un(A,B,C) :-
   1*A=9,
   1*B= -100,
   1*C=0,
   main_entry.
main__un1(A,B) :-
   5*B+1*C>=9,
   1*C>=4,
   -1*C>= -9,
   -1*B> -101,
   1*A+24*C>=116,
   main__un(C,A,B).
main_orig_main_exit(A) :-
   1*A>=4,
   1*B>=101,
   -1*A>= -9,
   24*A+1*C>=116,
   main__un(A,C,B).
main__un2(A,B) :-
   1*A>=1,
   -1*A> -102,
   1*B>= -100,
   1*A+ -1*C=1,
   main__un1(B,C).
main__un3(A,B) :-
   1*B>= -100,
   -1*B> -20,
   -1*A> -102,
   1*A>=1,
   main__un2(A,B).
main__un4(A,B,C) :-
   1*B>=20,
   -1*A> -102,
   1*A>=1,
   1*C=4,
   main__un2(A,B).
main__un2(A,B) :-
   -1*A+1*B>= -100,
   1*A+ -1*B> -20,
   -1*A> -102,
   1*A>=1,
   1*A+ -1*B+1*C=0,
   main__un3(A,C).
main__un(A,B,C) :-
   -1*C> -102,
   1*C>=1,
   1*B>=20,
   1*A=4,
   main__un4(C,B,A).
main_precall(A) :-
   -1*B>= -9,
   1*B>4,
   1*A=0,
   1*C=0,
   main_orig_main_exit(B).
main___VERIFIER_assert(A) :-
   1*A=0,
   main_precall(A).
main__un6 :-
   1*A=0,
   main___VERIFIER_assert(A).
main_verifier_error :-
   main__un6.

