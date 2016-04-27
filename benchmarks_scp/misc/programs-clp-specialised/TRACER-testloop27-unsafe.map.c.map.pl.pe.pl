new6(A,B,C,D) :-
   1*A=0,
   1*B=1,
   1*C=1.
new5(A,B,C) :-
   1*A=1,
   1*B=1,
   1*D=0,
   new6(D,A,B,C).
new4(A,B,C) :-
   -1*C>=0,
   1*A=0,
   1*B=1,
   1*D=1,
   new3(D,B,C).
new3(A,B,C) :-
   -1*C>=0,
   1*A=0,
   1*B=0,
   1*D=1,
   new4(A,D,C).
new3(A,B,C) :-
   1*A=1,
   1*B=1,
   new5(A,B,C).
new2 :-
   1*A=0,
   1*B=0,
   new3(A,B,C).
new1 :-
   new2.
false :-
   new1.

