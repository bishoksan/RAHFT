new8(A,B,C,D,E) :-
   1*C+ -1*D>=0,
   1*A=0,
   1*E=2.
new7(A,B,C,D) :-
   1*B+ -1*C>=0,
   1*D=2,
   1*E=0,
   new8(E,A,B,C,D).
new4(A,B,C,D) :-
   -1*B+1*C>=1,
   1*D=2,
   1*B+ -1*E= -1,
   new4(A,E,C,D).
new4(A,B,C,D) :-
   1*B+ -1*C>=0,
   1*D=2,
   new7(A,B,C,D).
new3(A,B,C,D) :-
   -1*A>=0,
   1*E=2,
   new4(A,B,C,E).
new2 :-
   -1*A>=0,
   new3(A,B,C,D).
new1 :-
   new2.
false :-
   new1.

