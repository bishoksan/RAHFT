false :-
   main_verifier_error.
gcd__1(A,B) :-
   1*B>=0,
   1*A>0.
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
   -1*A+1*C>=0,
   -1*A+1*B+ -1*C>=0,
   1*A>=1,
   1*D=1,
   1*E=0,
   1*F=0,
   1*B+ -1*C+ -1*G=0,
   gcd__8(B,C),
   gcd(D,E,F,G,C,A).
gcd___0(A,B,C) :-
   -1*A+1*B>=0,
   -1*A+ -1*B+1*C>=0,
   1*A>=1,
   1*D=1,
   1*E=0,
   1*F=0,
   1*B+ -1*C+1*G=0,
   gcd__11(B,C),
   gcd(D,E,F,B,G,A).
gcd__split(A,B,C) :-
   -1*A+1*B>=0,
   -1*A+1*C>=0,
   1*A>=1,
   gcd___0(A,B,C).
gcd(A,B,C,D,E,F) :-
   1*E+ -1*F>=0,
   1*F>=1,
   1*D+ -1*F>=0,
   1*A=1,
   1*B=0,
   1*C=0,
   gcd__split(F,D,E).
divides__1(A,B) :-
   1*B>=1,
   1*A>=0.
divides___0(A,B,C) :-
   1*C>=1,
   1*A=1,
   1*B=0,
   divides__1(B,C).
divides__3(A,B) :-
   1*B>=1,
   1*A>0,
   divides__1(A,B).
divides___0(A,B,C) :-
   -1*B+1*C>0,
   1*C>=1,
   1*B>0,
   1*A=0,
   divides__3(B,C).
divides__5(A,B) :-
   1*B>=1,
   1*A+ -1*B>=0,
   divides__3(A,B).
divides___0(A,B,C) :-
   1*A>=0,
   1*C>=1,
   1*B+ -1*C>=0,
   -1*A>= -1,
   1*A+1*B+ -1*C>0,
   1*D=1,
   1*E=0,
   1*F=0,
   1*B+ -1*C+ -1*G=0,
   divides__5(B,C),
   divides(D,E,F,C,G,A).
divides__split(A,B,C) :-
   1*A>=0,
   1*B>=0,
   1*C>=1,
   -1*A>= -1,
   1*A+1*B>0,
   divides___0(A,B,C).
divides(A,B,C,D,E,F) :-
   1*E+1*F>0,
   1*E>=0,
   1*F>=0,
   -1*F>= -1,
   1*D>=1,
   1*A=1,
   1*B=0,
   1*C=0,
   divides__split(F,E,D).
main_entry :-
   true.
main__un(A) :-
   1*A>=1,
   main_entry.
main__un2(A,B) :-
   1*B>0,
   1*A>=1,
   main__un(A).
main_postcall(A,B) :-
   1*B>0,
   1*A>=1,
   1*C=1,
   main__un2(A,B).
main_precall(A,B) :-
   1*B>0,
   1*A>=1,
   1*C=0,
   main__un2(A,B).
main_gcd(A,B) :-
   1*B>=1,
   1*A>0,
   main_precall(B,A).
main__un3 :-
   1*J>=1,
   1*I+ -1*J>=0,
   1*H+ -1*J>=0,
   1*A=1,
   1*B=0,
   1*C=0,
   1*D=1,
   1*E=0,
   1*F=0,
   1*G=0,
   main_postcall(I,H),
   gcd(A,B,C,I,H,J),
   divides(D,E,F,J,I,G).
main_verifier_error :-
   main__un3.
main__un4 :-
   1*B>=0,
   -1*A> -1,
   1*A>0,
   main_gcd(B,A).
main__un4 :-
   1*B>0,
   -1*A> -1,
   1*A>=0,
   main_gcd(A,B).
main__un5(A,B) :-
   1*B>=1,
   1*A>=1,
   main_gcd(A,B).
main_verifier_error :-
   main__un4.
main__un6(A,B) :-
   1*B>=1,
   1*A+ -1*B>0,
   main__un5(A,B).
main__un6(A,B) :-
   -1*A+1*B>0,
   1*A>=1,
   main__un5(A,B).
main__un7(A,B) :-
   -1*A+1*B>0,
   1*A>=1,
   main__un6(A,B).
main__un8(A,B) :-
   1*A+ -1*B>=0,
   1*B>=1,
   1*A+1*B>2,
   main__un6(A,B).
main_precall4(A,B) :-
   1*B>0,
   1*A>=1,
   1*C=0,
   1*A+1*B+ -1*D=0,
   main__un7(A,D).
main_gcd(A,B) :-
   1*B>0,
   1*A>=1,
   main_precall4(A,B).
main_precall6(A,B) :-
   1*A>=1,
   1*B>=0,
   2*A+1*B>2,
   1*C=0,
   1*A+1*B+ -1*D=0,
   main__un8(D,A).
main_gcd(A,B) :-
   1*B>=1,
   1*A>=0,
   1*A+2*B>2,
   main_precall6(B,A).

