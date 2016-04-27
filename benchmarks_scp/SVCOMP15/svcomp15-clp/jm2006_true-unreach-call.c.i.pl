false :-
   main_verifier_error.
verifier_error(A,B,C) :-
   A=0,
   B=0,
   C=0.
verifier_error(A,B,C) :-
   A=0,
   B=1,
   C=1.
verifier_error(A,B,C) :-
   A=1,
   B=0,
   C=1.
verifier_error(A,B,C) :-
   A=1,
   B=1,
   C=1.
main_entry :-
   true.
main__un(A,B,C,D) :-
   C=A,
   D=B,
   main_entry.
main__un2(A,B,C) :-
   main__un(A,B,C,D),
   D=0.
main__un1(A,B,C,D) :-
   main__un(A,B,C,D),
   D<0.
main__un1(A,B,C,D) :-
   main__un(A,B,C,D),
   D>0.
main__un(A,B,C,D) :-
   main__un1(E,B,C,F),
   A=E+ -1,
   D=F+ -1.
main__un3(A) :-
   main__un2(A,B,C),
   B=C.
main_precall(A) :-
   main__un3(B),
   B=0,
   A=1,
   C=0.
main_precall(A) :-
   main__un3(B),
   B<0,
   A=0,
   C=0.
main_precall(A) :-
   main__un3(B),
   B>0,
   A=0,
   C=0.
main___VERIFIER_assert(A) :-
   main_precall(A).
main__un4 :-
   A=0,
   main___VERIFIER_assert(A).
main_verifier_error :-
   main__un4.

