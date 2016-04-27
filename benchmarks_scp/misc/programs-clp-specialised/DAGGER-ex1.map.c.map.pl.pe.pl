new10(A,B,C,D,E,F) :-
   -1*E>=1,
   -5*A>=1,
   1*A+ -2*B+ -5*C=1,
   2*A+1*B+ -5*D=2,
   1*A+1*B+ -1*G=0,
   1*A+2*B+1*H=0,
   3*A+1*B+ -1*I=0,
   new3(A,G,H,I,E,F).
new10(A,B,C,D,E,F) :-
   -5*A>=1,
   1*E>=1,
   1*A+ -2*B+ -5*C=1,
   2*A+1*B+ -5*D=2,
   1*A+1*B+ -1*G=0,
   1*A+2*B+1*H=0,
   3*A+1*B+ -1*I=0,
   new3(A,G,H,I,E,F).
new10(A,B,C,D,E,F) :-
   -5*A>=1,
   1*A+ -2*B+ -5*C=1,
   2*A+1*B+ -5*D=2,
   1*E=0,
   1*A+ -1*B+1*G=0,
   3*A+ -2*B+ -1*H=0,
   1*A+1*B+ -1*I=0,
   new3(A,G,H,I,E,F).
new8(A,B,C,D,E,F,G) :-
   1*A=0,
   1*G=0.
new7(A,B,C,D,E,F) :-
   -1*C+ -2*D>=1,
   1*F=0,
   1*G=0,
   new8(G,A,B,C,D,E,F).
new5(A,B,C,D,E,F) :-
   -5*A>=1,
   1*A+ -2*B+ -5*C=1,
   2*A+1*B+ -5*D=2,
   new10(A,B,C,D,G,F).
new4(A,B,C,D,E,F) :-
   -1*F>=1,
   -5*C+ -10*D>=6,
   1*C+2*D+ -1*G= -1,
   2*C+ -1*D+1*H=0,
   new5(G,H,C,D,E,F).
new4(A,B,C,D,E,F) :-
   -5*C+ -10*D>=6,
   1*F>=1,
   1*C+2*D+ -1*G= -1,
   2*C+ -1*D+1*H=0,
   new5(G,H,C,D,E,F).
new4(A,B,C,D,E,F) :-
   -1*C+ -2*D>=1,
   1*F=0,
   new7(A,B,C,D,E,F).
new3(A,B,C,D,E,F) :-
   -1*C+ -2*D>=1,
   new4(A,B,C,D,E,G).

