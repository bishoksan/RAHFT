new17(A,B,C,D,E,F,G) :-
   -3*A+1*B>=1,
   1*A>=0,
   20*A+ -10*B+1*C=0,
   20*A+ -10*B+1*D=0,
   1*E=0,
   1*F=0,
   new3(A,B,C,D,E,F,G).
new15(A,B,C,D,E,F,G) :-
   -3*A+1*B>=1,
   1*A>=4,
   20*A+ -10*B+1*C=0,
   20*A+ -10*B+1*D=0,
   1*F=0,
   1*A+ -1*H= -1,
   1*B+ -1*I= -3,
   20*A+ -10*B+1*J=10,
   20*A+ -10*B+1*K=10,
   new3(H,I,J,K,E,F,G).
new15(A,B,C,D,E,F,G) :-
   -1*A>= -3,
   -3*A+1*B>=1,
   1*A>=0,
   20*A+ -10*B+1*C=0,
   20*A+ -10*B+1*D=0,
   1*F=0,
   new3(A,B,C,D,E,F,G).
new14(A,B,C,D,E,F,G) :-
   -1*E>=1,
   -3*A+1*B>=1,
   1*A>=0,
   20*A+ -10*B+1*C=0,
   20*A+ -10*B+1*D=0,
   1*F=0,
   new15(A,B,C,D,E,F,G).
new14(A,B,C,D,E,F,G) :-
   1*E>=1,
   -3*A+1*B>=1,
   1*A>=0,
   20*A+ -10*B+1*C=0,
   20*A+ -10*B+1*D=0,
   1*F=0,
   new15(A,B,C,D,E,F,G).
new14(A,B,C,D,E,F,G) :-
   -3*A+1*B>=1,
   1*A>=0,
   20*A+ -10*B+1*C=0,
   20*A+ -10*B+1*D=0,
   1*E=0,
   1*F=0,
   new17(A,B,C,D,E,F,G).
new13(A,B,C,D,E,F,G) :-
   -3*A+1*B>=1,
   1*A>=0,
   20*A+ -10*B+1*C=0,
   20*A+ -10*B+1*D=0,
   1*F=0,
   new14(A,B,C,D,H,F,G).
new10(A,B,C,D,E,F,G) :-
   -1*F>=1,
   -3*A+1*B>=2,
   1*A>=0,
   20*A+ -10*B+1*C=0,
   20*A+ -10*B+1*D=0,
   1*A+ -1*H= -1,
   1*B+ -1*I= -2,
   new3(H,I,C,D,E,F,G).
new10(A,B,C,D,E,F,G) :-
   -3*A+1*B>=2,
   1*F>=1,
   1*A>=0,
   20*A+ -10*B+1*C=0,
   20*A+ -10*B+1*D=0,
   1*A+ -1*H= -1,
   1*B+ -1*I= -2,
   new3(H,I,C,D,E,F,G).
new10(A,B,C,D,E,F,G) :-
   -3*A+1*B>=1,
   1*A>=0,
   20*A+ -10*B+1*C=0,
   20*A+ -10*B+1*D=0,
   1*F=0,
   new13(A,B,C,D,E,F,G).
new8(A,B,C,D,E,F,G,H) :-
   -2*B+1*C>=1,
   1*B>=0,
   1*A=0,
   20*B+ -10*C+1*D=0,
   20*B+ -10*C+1*E=0,
   1*H=0.
new7(A,B,C,D,E,F,G) :-
   -3*A+1*B>=1,
   1*A>=0,
   20*A+ -10*B+1*C=0,
   20*A+ -10*B+1*D=0,
   1*G=0,
   1*H=0,
   new8(H,A,B,C,D,E,F,G).
new5(A,B,C,D,E,F,G) :-
   -3*A+1*B>=1,
   1*A>=0,
   20*A+ -10*B+1*C=0,
   20*A+ -10*B+1*D=0,
   new10(A,B,C,D,E,H,G).
new4(A,B,C,D,E,F,G) :-
   -1*G>=1,
   -3*A+1*B>=1,
   1*A>=0,
   20*A+ -10*B+1*C=0,
   20*A+ -10*B+1*D=0,
   new5(A,B,C,D,E,F,G).
new4(A,B,C,D,E,F,G) :-
   1*G>=1,
   -3*A+1*B>=1,
   1*A>=0,
   20*A+ -10*B+1*C=0,
   20*A+ -10*B+1*D=0,
   new5(A,B,C,D,E,F,G).
new4(A,B,C,D,E,F,G) :-
   -3*A+1*B>=1,
   1*A>=0,
   20*A+ -10*B+1*C=0,
   20*A+ -10*B+1*D=0,
   1*G=0,
   new7(A,B,C,D,E,F,G).
new3(A,B,C,D,E,F,G) :-
   -3*A+1*B>=1,
   1*A>=0,
   20*A+ -10*B+1*C=0,
   20*A+ -10*B+1*D=0,
   new4(A,B,C,D,E,F,H).
