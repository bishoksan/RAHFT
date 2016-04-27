new9(A,B,C,D,E) :-
   -1*C>= -3,
   1*C>=2,
   1*A=0,
   1*B=2.
new8(A,B,C,D) :-
   -1*B>= -3,
   1*B>=2,
   1*A=2,
   1*E=0,
   new9(E,A,B,C,D).
new6(A,B,C,D) :-
   -1*B>= -3,
   1*B>=2,
   1*A=2,
   new8(A,B,C,D).
new4(A,B,C,D) :-
   -1*C>=0,
   -1*B>= -3,
   1*B>=2,
   1*E=2,
   new6(E,B,C,D).
new3(A,B,C,D) :-
   -1*C>=0,
   1*D>=1,
   1*E=2,
   new4(A,E,C,D).
new3(A,B,C,D) :-
   -1*D>=0,
   -1*C>=0,
   1*E=3,
   new4(A,E,C,D).
new2 :-
   -1*A>=0,
   new3(B,C,A,D).
new1 :-
   new2.
false :-
   new1.

