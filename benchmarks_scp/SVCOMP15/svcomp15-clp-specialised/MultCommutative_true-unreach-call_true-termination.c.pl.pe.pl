false :-
   main_verifier_error.
mult__1(A,B) :-
   -1*B>= -46340.
mult__3(A,B) :-
   -1*B>= -46340,
   -1*A>0,
   mult__1(A,B).
mult__6(A,B) :-
   -1*B>= -46340,
   1*A>=0,
   mult__1(A,B).
mult___0(A,B,C) :-
   -1*C>= -46340,
   -1*B>0,
   1*D=1,
   1*E=0,
   1*F=0,
   1*B+1*G=0,
   mult__3(B,C),
   mult(D,E,F,C,G,A).
mult___0(A,B,C) :-
   -1*C>= -46340,
   1*A=0,
   1*B=0,
   mult__6(B,C).
mult__8(A,B) :-
   -1*B>= -46340,
   1*A>0,
   mult__6(A,B).
mult___0(A,B,C) :-
   -1*C>= -46340,
   1*B>0,
   1*D=1,
   1*E=0,
   1*F=0,
   1*B+ -1*G=1,
   1*A+ -1*C+ -1*H=0,
   mult__8(B,C),
   mult(D,E,F,C,G,H).
mult__split(A,B,C) :-
   -1*C>= -46340,
   mult___0(A,B,C).
mult(A,B,C,D,E,F) :-
   -1*D>= -46340,
   1*A=1,
   1*B=0,
   1*C=0,
   mult__split(F,E,D).
main_entry :-
   true.
main__un(A) :-
   -1*A>= -46340,
   main_entry.
main__un1(A,B) :-
   -1*B>= -46340,
   -1*A>= -46340,
   main__un(A).
main__un2 :-
   1*J>0,
   -1*J>= -46340,
   -1*I>= -46340,
   -1*G+1*H>0,
   1*I>0,
   1*A=1,
   1*B=0,
   1*C=0,
   1*D=1,
   1*E=0,
   1*F=0,
   main__un1(I,J),
   mult(A,B,C,I,J,G),
   mult(D,E,F,J,I,H).
main__un2 :-
   1*I>0,
   1*J>0,
   -1*J>= -46340,
   -1*I>= -46340,
   1*G+ -1*H>0,
   1*A=1,
   1*B=0,
   1*C=0,
   1*D=1,
   1*E=0,
   1*F=0,
   main__un1(I,J),
   mult(A,B,C,I,J,G),
   mult(D,E,F,J,I,H).
main_verifier_error :-
   main__un2.

