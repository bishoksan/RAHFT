new19(A,B,C,D,E) :-
   1*A=0.
new10(A,B,C,D) :-
   -1*A>=0,
   1*E=0,
   new19(E,A,B,C,D).
new9(A,B,C,D) :-
   -1*D>=1,
   -1*A>=1,
   1*A+ -1*E= -1,
   new10(E,B,C,D).
new9(A,B,C,D) :-
   -1*A>=1,
   1*D>=1,
   1*A+ -1*E= -1,
   new10(E,B,C,D).
new7(A,B,C,D) :-
   -1*A>=1,
   new9(A,B,C,E).
new4(A,B,C,D,E) :-
   -1*B>=1,
   1*A=1,
   new7(B,C,D,E).

