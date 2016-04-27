new10(A,B,C,D) :-
   1*A=0,
   1*B=0,
   1*C=0,
   1*D=0.
new9(A,B,C) :-
   1*A=0,
   1*B=0,
   1*C=0,
   1*D=0,
   new10(D,A,B,C).
new7(A,B,C) :-
   1*A>=1,
   1*A+ -1*B=0,
   1*C=0,
   1*A+ -1*D=1,
   1*A+ -1*E=1,
   new7(D,E,C).
new7(A,B,C) :-
   1*A=0,
   1*B=0,
   1*C=0,
   new9(A,B,C).
new4(A,B,C) :-
   -1*C>=1,
   1*A>=0,
   1*A+ -1*B=0,
   1*A+ -1*D= -1,
   1*A+ -1*E= -1,
   new3(D,E,C).
new4(A,B,C) :-
   1*C>=1,
   1*A>=0,
   1*A+ -1*B=0,
   1*A+ -1*D= -1,
   1*A+ -1*E= -1,
   new3(D,E,C).
new4(A,B,C) :-
   1*A>=0,
   1*A+ -1*B=0,
   1*C=0,
   new7(A,B,C).
new3(A,B,C) :-
   1*A>=0,
   1*A+ -1*B=0,
   new4(A,B,D).
new2 :-
   1*A=0,
   1*B=0,
   new3(A,B,C).
new1 :-
   new2.
false :-
   new1.

