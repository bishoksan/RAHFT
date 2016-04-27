new8(A,B,C,D) :-
   1*C>=1,
   1*B>= -1,
   1*A=0.
new6(A,B,C) :-
   1*A= -1,
   1*B=1,
   1*D=0,
   new8(D,A,B,C).
new4(A,B,C) :-
   1*A= -1,
   1*B=1,
   1*D=3,
   new6(A,B,D).
new4(A,B,C) :-
   -1*B>=0,
   1*A= -1,
   1*D=1,
   new6(A,D,C).
new3(A,B,C) :-
   -1*B>= -1,
   -1*A>=0,
   1*D= -1,
   new4(D,B,C).
new2 :-
   -1*B>= -1,
   -1*A>=0,
   new3(A,B,C).
new1 :-
   new2.
false :-
   new1.

