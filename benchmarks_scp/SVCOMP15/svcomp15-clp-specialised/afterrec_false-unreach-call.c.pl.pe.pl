false :-
   main_verifier_error.
f__1(A) :-
   -1*A>= -3,
   1*A>=2.
f__ret(A) :-
   -1*A> -3,
   1*A>=2,
   f__1(A).
f(A,B,C,D) :-
   -1*D> -3,
   1*D>=2,
   1*A=1,
   1*B=0,
   1*C=0,
   f__ret(D).
main_entry :-
   true.
main_precall :-
   1*A=0,
   main_entry.
main_f(A) :-
   1*A=4,
   main_precall.
main__un(A) :-
   -1*A>= -4,
   1*A>=3,
   main_f(A).
main_postcall1(A) :-
   -1*A>= -3,
   1*A>=2,
   1*B=1,
   1*A+ -1*C= -1,
   main__un(C).
main_precall2(A) :-
   -1*A>= -3,
   1*A>=2,
   1*B=0,
   1*A+ -1*C= -1,
   main__un(C).
main_f(A) :-
   -1*A>= -3,
   1*A>=2,
   main_precall2(A).
main_verifier_error :-
   -1*D> -3,
   1*D>=2,
   1*A=1,
   1*B=0,
   1*C=0,
   main_postcall1(D),
   f(A,B,C,D).

