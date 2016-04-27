new14(A,B,C,D,E) :-
   -1*B>= -5,
   1*B>=0,
   1*A=0,
   2*B+1*C=10,
   1*E=0.
new9(A,B,C,D) :-
   1*A=0,
   1*B=10,
   1*E=0,
   new8(A,B,C,E).
new8(A,B,C,D) :-
   -1*A>= -5,
   1*A>=0,
   2*A+1*B=10,
   1*D=0,
   1*E=0,
   new14(E,A,B,C,D).
new5(A,B,C,D) :-
   1*A=5,
   1*B=0,
   1*E=0,
   new8(A,B,C,E).
new5(A,B,C,D) :-
   1*A=0,
   1*B=10,
   new9(A,B,C,D).
new4(A,B,C,D) :-
   -1*C>=1,
   1*A=0,
   1*B=0,
   1*E=5,
   new5(E,B,C,D).
new4(A,B,C,D) :-
   1*C>=1,
   1*A=0,
   1*B=0,
   1*E=5,
   new5(E,B,C,D).
new4(A,B,C,D) :-
   1*A=0,
   1*B=0,
   1*C=0,
   1*E=10,
   new5(A,E,C,D).
new3(A,B,C,D) :-
   1*A=0,
   1*B=0,
   new4(A,B,E,D).
new2 :-
   1*A=0,
   1*B=0,
   new3(B,A,C,D).
new1 :-
   new2.
false :-
   new1.

