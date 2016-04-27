new6(A,B,C,D) :-
   1*C>=0,
   1*B=0.
new5(A,B,C) :-
   -1*C>=0,
   1*B>=0,
   1*D=0,
   new6(A,D,B,C).
new3(A,B,C) :-
   -1*C>=0,
   1*B>=0,
   new5(A,B,C).

