new11(A,B,C,D,E,F,G,H,I,J,K) :-
   1*F>=1,
   -1*E+1*F>=0,
   -1*B>= -2,
   1*B>=1,
   1*A=0,
   1*J+ -1*K=0.
new10(A,B,C,D,E,F,G,H,I,J) :-
   -1*D>= -1,
   -1*A>= -2,
   1*A>=1,
   1*E=1,
   1*I+ -1*J=0,
   1*K=0,
   new11(K,A,B,C,D,E,F,G,H,I,J).
new8(A,B,C,D,E,F,G,H,I,J) :-
   -1*D>= -1,
   -1*A>= -2,
   1*A>=1,
   1*E=1,
   1*I+ -1*J=0,
   new10(A,B,C,D,E,F,G,H,I,J).
new7(A,B) :-
   -1*E>= -1,
   1*A=0,
   1*B=0,
   1*C=2,
   1*D=1,
   new8(C,F,G,E,D,H,I,J,A,B).
new5(A,B) :-
   -1*E>= -1,
   1*A+ -1*B=0,
   1*C=1,
   1*D=1,
   new8(C,F,G,E,D,H,I,J,A,B).
new4(A,B) :-
   -1*A>=1,
   1*A+ -1*B=0,
   new5(A,B).
new4(A,B) :-
   1*A>=1,
   1*A+ -1*B=0,
   new5(A,B).
new4(A,B) :-
   1*A=0,
   1*B=0,
   new7(A,B).
new3(A,B) :-
   1*C+ -1*D=0,
   new4(D,C).
new2 :-
   new3(A,B).
new1 :-
   new2.
false :-
   new1.

