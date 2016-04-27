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
f(A,B,C,D) :-
   A=1,
   B=1,
   C=1.
f(A,B,C,D) :-
   A=0,
   B=1,
   C=1.
f(A,B,C,D) :-
   A=0,
   B=0,
   C=0.
f__1(A) :-
   true.
f__ret(A) :-
   f__1(A),
   A<3.
f(A,B,C,D) :-
   A=1,
   B=0,
   C=0,
   f__ret(D).
main_entry :-
   true.
main_precall :-
   main_entry,
   A=0.
main_f(A) :-
   main_precall,
   A=4.
main__un(A) :-
   main_f(A),
   A>=3.
main_postcall1(A) :-
   B=1,
   main__un(C),
   A=C+ -1.
main_precall2(A) :-
   main__un(B),
   C=0,
   A=B+ -1.
main_f(A) :-
   main_precall2(A).
main_verifier_error :-
   A=1,
   B=0,
   C=0,
   main_postcall1(D),
   f(A,B,C,D).

