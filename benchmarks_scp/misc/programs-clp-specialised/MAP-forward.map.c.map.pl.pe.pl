new10(A,B,C,D,E,F) :-
   3*B+ -3*C>=1,
   -1*B+1*D>=0,
   2*B+ -1*D>=0,
   3*B+ -1*D+ -1*E=0.
new7(A,B,C,D,E,F) :-
   2*B+ -1*D>=0,
   -1*B+1*D>=0,
   3*B+ -3*C>=1,
   3*B+ -1*D+ -1*E=0,
   new10(A,B,C,D,E,F).
new4(A,B,C,D,E,F,G) :-
   -3*D>=1,
   1*A=1,
   1*H=0,
   1*I=0,
   1*J=0,
   new7(B,H,D,I,J,G).

