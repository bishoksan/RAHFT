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
main__un(A,B) :-
   main_entry,
   B<1001,
   A=0.
main__un1(A,B) :-
   main__un(A,B),
   A<B.
main__un2(A,B) :-
   main__un(C,B),
   C>=B,
   A=0.
main__un(A,B) :-
   main__un1(C,B),
   A=C+1.
main__un3(A,B) :-
   main__un2(A,B),
   A<B.
main_postcall(A,B,C) :-
   D=1,
   main__un3(B,C),
   E=0,
   A=1.
main_postcall(A,B,C) :-
   D=1,
   main__un3(B,C),
   E<0,
   A=0.
main_postcall(A,B,C) :-
   D=1,
   main__un3(B,C),
   E>0,
   A=0.
main_precall(A) :-
   main__un3(B,C),
   D=0,
   A=1,
   E=0.
main_precall(A) :-
   main__un3(B,C),
   D<0,
   A=0,
   E=0.
main_precall(A) :-
   main__un3(B,C),
   D>0,
   A=0,
   E=0.
main___VERIFIER_assert(A) :-
   main_precall(A).
main__un2(A,B) :-
   main_postcall(C,D,B),
   p____VERIFIER_assert(1,0,0,C),
   A=D+1.
main__un4 :-
   A=0,
   main___VERIFIER_assert(A).
main_verifier_error :-
   main__un4.

