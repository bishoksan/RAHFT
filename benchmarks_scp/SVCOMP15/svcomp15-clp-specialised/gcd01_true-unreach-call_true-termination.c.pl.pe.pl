false :-
   main_verifier_error.
gcd__1(A,B) :-
   1*B>=0,
   1*A>0.
gcd___0(A,B,C) :-
   -1*B> -1,
   1*C>=0,
   1*B>0,
   1*A=0,
   gcd__1(B,C).
gcd___0(A,B,C) :-
   -1*C> -1,
   1*C>=0,
   1*B>0,
   1*A=0,
   gcd__1(B,C).
gcd__4(A,B) :-
   1*B>=1,
   1*A>=1,
   gcd__1(A,B).
gcd___0(A,B,C) :-
   1*A>=1,
   1*A+ -1*B=0,
   1*A+ -1*C=0,
   gcd__4(B,C).
gcd__6(A,B) :-
   -1*A+1*B>0,
   1*A>=1,
   gcd__4(A,B).
gcd__6(A,B) :-
   1*B>=1,
   1*A+ -1*B>0,
   gcd__4(A,B).
gcd__8(A,B) :-
   1*B>=1,
   1*A+ -1*B>0,
   gcd__6(A,B).
gcd__11(A,B) :-
   1*A>=1,
   -1*A+1*B>=0,
   1*A+1*B>2,
   gcd__6(A,B).
gcd___0(A,B,C) :-
   1*B+ -1*C>0,
   1*C>=1,
   -1*A+1*C>=0,
   -1*A+1*B+ -1*C>=0,
   1*A>=0,
   1*D=1,
   1*E=0,
   1*F=0,
   1*B+ -1*C+ -1*G=0,
   gcd__8(B,C),
   gcd(D,E,F,G,C,A).
gcd___0(A,B,C) :-
   1*B>=1,
   -1*A+1*B>=0,
   1*C>1,
   -1*A+ -1*B+1*C>=0,
   1*A>=0,
   1*D=1,
   1*E=0,
   1*F=0,
   1*B+ -1*C+1*G=0,
   gcd__11(B,C),
   gcd(D,E,F,B,G,A).
gcd__split(A,B,C) :-
   1*B+1*C>1,
   1*B>0,
   -1*A+1*B>=0,
   -1*A+1*C>=0,
   1*A>=0,
   gcd___0(A,B,C).
gcd(A,B,C,D,E,F) :-
   1*D>0,
   1*D+ -1*F>=0,
   1*E+ -1*F>=0,
   1*F>=0,
   1*D+1*E>1,
   1*A=1,
   1*B=0,
   1*C=0,
   gcd__split(F,D,E).
main_entry :-
   true.
main__un(A) :-
   1*A>=1,
   main_entry.
main__un1(A,B) :-
   1*B>=1,
   1*A>=1,
   main__un(A).
main__un2 :-
   1*F>=1,
   1*E>=1,
   -1*D> -1,
   1*D>=0,
   1*A=1,
   1*B=0,
   1*C=0,
   main__un1(E,F),
   gcd(A,B,C,E,F,D).
main_verifier_error :-
   main__un2.

