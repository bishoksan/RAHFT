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
p____VERIFIER_assert(A,B,C,D) :-
   A=1,
   B=1,
   C=1.
p____VERIFIER_assert(A,B,C,D) :-
   A=0,
   B=1,
   C=1.
p____VERIFIER_assert(A,B,C,D) :-
   A=0,
   B=0,
   C=0.
p____VERIFIER_assert__1(A) :-
   true.
p____VERIFIER_assert__ret(A) :-
   p____VERIFIER_assert__1(A),
   A<0.
p____VERIFIER_assert__ret(A) :-
   p____VERIFIER_assert__1(A),
   A>0.
p____VERIFIER_assert(A,B,C,D) :-
   A=1,
   B=0,
   C=0,
   p____VERIFIER_assert__ret(D).
main_entry :-
   true.
main__un(A,B,C) :-
   main_entry,
   A=0,
   B=0.
main__un1(A,B,C) :-
   main__un(A,B,C),
   B<C.
main_orig_main_exit(A,B) :-
   main__un(A,C,B),
   C>=B.
main__un(A,B,C) :-
   main__un1(D,E,C),
   F=0,
   A=D,
   B=E+1.
main__un(A,B,C) :-
   main__un1(D,E,C),
   F<0,
   A=E,
   B=E+1.
main__un(A,B,C) :-
   main__un1(D,E,C),
   F>0,
   A=E,
   B=E+1.
main_postcall(A,B,C) :-
   D=1,
   main_orig_main_exit(A,C),
   C<1,
   E=1.
main_postcall(A,B,C) :-
   D=1,
   main_orig_main_exit(A,C),
   C>=1,
   E=0.
main_precall(A) :-
   main_orig_main_exit(B,C),
   C<1,
   D=1,
   E=0.
main_precall(A) :-
   main_orig_main_exit(B,C),
   C>=1,
   D=0,
   E=0.
main___VERIFIER_assert(A) :-
   main_precall(A).
main_precall2(A) :-
   main_postcall(B,C,D),
   p____VERIFIER_assert(1,0,0,C),
   E=0,
   A=1,
   D<1.
main_precall2(A) :-
   main_postcall(B,C,D),
   p____VERIFIER_assert(1,0,0,C),
   E=0,
   A=1,
   B<D.
main_precall2(A) :-
   main_postcall(B,C,D),
   p____VERIFIER_assert(1,0,0,C),
   E=0,
   D>=1,
   B>=D,
   A=0.
main___VERIFIER_assert(A) :-
   main_precall2(A).
main__un2 :-
   main___VERIFIER_assert(A),
   A=0.
main_verifier_error :-
   main__un2.

