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
gcd(A,B,C,D,E,F) :-
   A=1,
   B=1,
   C=1.
gcd(A,B,C,D,E,F) :-
   A=0,
   B=1,
   C=1.
gcd(A,B,C,D,E,F) :-
   A=0,
   B=0,
   C=0.
gcd__1(A,B) :-
   true.
gcd__4(A,B) :-
   gcd__1(A,B),
   A>=1,
   B>=1.
gcd___0(A,B,C) :-
   B=A,
   gcd__4(B,C),
   B=C.
gcd__6(A,B) :-
   gcd__4(A,B),
   A<B.
gcd__6(A,B) :-
   gcd__4(A,B),
   A>B.
gcd__8(A,B) :-
   gcd__6(A,B),
   A>B.
gcd__11(A,B) :-
   gcd__6(A,B),
   A=<B.
gcd___0(A,B,C) :-
   D=1,
   E=0,
   F=0,
   G=B-C,
   gcd__8(B,C),
   gcd(D,E,F,G,C,A).
gcd___0(A,B,C) :-
   D=1,
   E=0,
   F=0,
   G=C-B,
   gcd__11(B,C),
   gcd(D,E,F,B,G,A).
gcd__split(A,B,C) :-
   gcd___0(A,B,C).
gcd(A,B,C,D,E,F) :-
   A=1,
   B=0,
   C=0,
   gcd__split(F,D,E).
divides(A,B,C,D,E,F) :-
   A=1,
   B=1,
   C=1.
divides(A,B,C,D,E,F) :-
   A=0,
   B=1,
   C=1.
divides(A,B,C,D,E,F) :-
   A=0,
   B=0,
   C=0.
divides__1(A,B) :-
   true.
divides___0(A,B,C) :-
   divides__1(B,C),
   B=0,
   A=1.
divides__3(A,B) :-
   divides__1(A,B),
   A<0.
divides__3(A,B) :-
   divides__1(A,B),
   A>0.
divides___0(A,B,C) :-
   divides__3(B,C),
   B<C,
   A=0.
divides__5(A,B) :-
   divides__3(A,B),
   A>=B.
divides___0(A,B,C) :-
   D=1,
   E=0,
   F=0,
   G=B-C,
   divides__5(B,C),
   divides(D,E,F,C,G,A).
divides__split(A,B,C) :-
   divides___0(A,B,C).
divides(A,B,C,D,E,F) :-
   A=1,
   B=0,
   C=0,
   divides__split(F,E,D).
main_entry :-
   true.
main__un(A) :-
   main_entry,
   A>=1.
main__un2(A,B) :-
   main__un(A),
   A>0,
   B>0.
main_postcall(A,B) :-
   C=1,
   main__un2(A,B).
main_precall(A,B) :-
   main__un2(A,B),
   C=0.
main_gcd(A,B) :-
   main_precall(B,A).
main__un3 :-
   main_postcall(A,B),
   gcd(1,0,0,A,B,C),
   divides(1,0,0,C,A,D),
   D=0.
main_verifier_error :-
   main__un3.
main__un4 :-
   main_gcd(A,B),
   B<1.
main__un4 :-
   main_gcd(A,B),
   A<1.
main__un5(A,B) :-
   main_gcd(A,B),
   B>=1,
   A>=1.
main_verifier_error :-
   main__un4.
main__un6(A,B) :-
   main__un5(A,B),
   B<A.
main__un6(A,B) :-
   main__un5(A,B),
   B>A.
main__un7(A,B) :-
   main__un6(A,B),
   B>A.
main__un8(A,B) :-
   main__un6(A,B),
   B=<A.
main_precall4(A,B) :-
   main__un7(A,C),
   D=0,
   B=C-A.
main_gcd(A,B) :-
   main_precall4(A,B).
main_precall6(A,B) :-
   main__un8(C,A),
   D=0,
   B=C-A.
main_gcd(A,B) :-
   main_precall6(B,A).

