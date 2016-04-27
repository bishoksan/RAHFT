new24(A,B,C,D,E) :-
   1*A=0,
   1*B=7.
new21(A,B,C,D) :-
   1*A=7,
   1*E=0,
   new24(E,A,B,C,D).
new20(A,B,C,D) :-
   -1*D>=1,
   1*A=3,
   1*E=7,
   new21(E,B,C,D).
new20(A,B,C,D) :-
   1*D>=1,
   1*A=3,
   1*E=7,
   new21(E,B,C,D).
new18(A,B,C,D) :-
   1*A=3,
   new20(A,B,C,E).
new16(A,B,C,D,E) :-
   1*A=1,
   1*B=3,
   new18(B,C,D,E).
new13(A,B,C,D) :-
   1*A=3,
   1*E=1,
   new16(E,A,B,C,D).
new12(A,B,C,D) :-
   -1*C>=1,
   1*A=1,
   1*E=3,
   new13(E,B,C,D).
new12(A,B,C,D) :-
   1*C>=1,
   1*A=1,
   1*E=3,
   new13(E,B,C,D).
new10(A,B,C,D) :-
   1*A=1,
   new12(A,B,E,D).
new8(A,B,C,D,E) :-
   1*A=1,
   1*B=1,
   new10(B,C,D,E).
new5(A,B,C,D) :-
   1*A=1,
   1*E=1,
   new8(E,A,B,C,D).
new4(A,B,C,D) :-
   -1*B>=1,
   1*A=0,
   1*E=1,
   new5(E,B,C,D).
new4(A,B,C,D) :-
   1*B>=1,
   1*A=0,
   1*E=1,
   new5(E,B,C,D).
new3(A,B,C,D) :-
   1*A=0,
   new4(A,E,C,D).
new2 :-
   1*A=0,
   new3(A,B,C,D).
new1 :-
   new2.
false :-
   new1.

