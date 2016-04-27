new17(A,B,C,D) :-
   1*B>=2,
   1*A=0.
new14(A,B,C,D,E,F,G) :-
   1*A=5,
   1*B=1,
   1*C=0,
   1*D=1,
   1*E= -1,
   1*F=0,
   1*H=6,
   1*I=1,
   new9(H,F,I).
new12(A,B,C) :-
   1*A= -1,
   1*B=0,
   1*D=1,
   1*E=5,
   1*F=1,
   1*G=0,
   new14(E,F,G,D,A,B,C).
new11(A,B,C,D,E,F,G) :-
   -1*A>=0,
   1*B=1,
   1*C=0,
   1*D=1,
   1*A+ -1*E=0,
   1*H= -1,
   new12(H,C,G).
new10(A,B,C) :-
   -1*A>=0,
   1*A+ -1*D=0,
   1*E=1,
   1*F=1,
   1*G=0,
   new11(D,F,G,E,A,B,C).
new9(A,B,C) :-
   1*A>=2,
   1*D=0,
   new17(D,A,B,C).
new7(A,B,C) :-
   1*A>=1,
   1*A+ -1*D= -1,
   new9(D,B,C).
new7(A,B,C) :-
   -1*A>=0,
   new10(A,B,C).
new4(A,B,C,D) :-
   1*A=1,
   new7(B,C,D).
new3(A,B,C) :-
   1*A>=1,
   1*D=1,
   new4(D,A,B,C).
new2 :-
   1*A>=1,
   new3(A,B,C).
new1 :-
   new2.
false :-
   new1.

