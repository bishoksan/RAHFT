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
gcd___0(A,B,C) :-
   gcd__1(B,C),
   B<1,
   A=0.
gcd___0(A,B,C) :-
   gcd__1(B,C),
   C<1,
   A=0.
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
main_entry :-
   true.
main__un(A) :-
   main_entry,
   A>=1.
main__un1(A,B) :-
   main__un(A),
   B>=1.
main__un2 :-
   main__un1(A,B),
   gcd(1,0,0,A,B,C),
   C<1,
   A>0,
   B>0.
main_verifier_error :-
   main__un2.

