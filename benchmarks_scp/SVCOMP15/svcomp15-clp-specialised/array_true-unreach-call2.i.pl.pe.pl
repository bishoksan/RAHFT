false :-
   main_verifier_error.
main_entry :-
   true.
main__un(A) :-
   1*A=0,
   main_entry.
main__un2(A) :-
   -1*A> -2048,
   1*A>=0,
   main__un(A).
main_orig_main_exit :-
   -1*A> -2049,
   1*A>=2048,
   main__un(A).
main__un(A) :-
   -1*A> -2049,
   1*A>=1,
   1*A+ -1*B=1,
   main__un2(B).
main_precall(A) :-
   -1*B+1*C>0,
   1*A=0,
   1*D=0,
   main_orig_main_exit.
main_precall(A) :-
   1*B+ -1*C>0,
   1*A=0,
   1*D=0,
   main_orig_main_exit.
main___VERIFIER_assert(A) :-
   1*A=0,
   main_precall(A).
main__un3 :-
   1*A=0,
   main___VERIFIER_assert(A).
main_verifier_error :-
   main__un3.

