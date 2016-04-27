new25(A,B,C,D) :-
   1*D>=1,
   1*C>=1,
   1*A=0,
   1*B=0.
new23(A,B,C) :-
   -1*B+1*C>=0,
   1*B>=1,
   1*A=0,
   1*D=0,
   new25(D,A,B,C).
new21(A,B,C,D) :-
   -1*C+1*D>=0,
   1*C>=1,
   1*A=1,
   1*B=0,
   new23(B,C,D).
new20(A,B,C) :-
   -1*B+1*C>=0,
   1*B>=1,
   1*A=0,
   1*D=1,
   new21(D,A,B,C).
new13(A,B,C,D) :-
   1*D>=1,
   1*C>=1,
   1*A=0.
new11(A,B,C) :-
   -1*B+1*C>=0,
   1*B>=1,
   1*D=0,
   new13(D,A,B,C).
new9(A,B,C,D) :-
   -1*C+1*D>=0,
   1*C>=1,
   1*A=1,
   new11(B,C,D).
new8(A,B,C) :-
   -1*B+1*C>=0,
   1*B>=1,
   1*D=1,
   new9(D,A,B,C).

