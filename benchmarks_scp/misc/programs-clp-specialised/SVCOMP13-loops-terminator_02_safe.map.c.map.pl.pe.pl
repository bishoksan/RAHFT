new23(A,B,C,D,E,F,G,H,I,J) :-
   -1*F+1*G>=0,
   1*B+ -1*C+ -1*F+1*G>=0,
   1*A=0,
   1*D+ -1*E=0,
   1*J=0.
new20(A,B,C,D,E,F,G,H,I) :-
   1*E>=101,
   -1*E+1*F>=0,
   -1*A>= -99,
   1*A+ -1*B+ -1*E+1*F>=0,
   1*C+ -1*D=0,
   1*J=0,
   new19(A,B,C,D,E,F,G,H,J).
new19(A,B,C,D,E,F,G,H,I) :-
   -1*E+1*F>=0,
   1*A+ -1*B+ -1*E+1*F>=0,
   1*C+ -1*D=0,
   1*I=0,
   1*J=0,
   new23(J,A,B,C,D,E,F,G,H,I).
new18(A,B,C,D,E,F,G,H,I) :-
   1*E>=101,
   -1*E+1*F>=0,
   -1*A>= -99,
   1*A+ -1*B+ -1*E+1*F>=0,
   1*C+ -1*D=0,
   new20(A,B,C,D,E,F,G,H,I).
