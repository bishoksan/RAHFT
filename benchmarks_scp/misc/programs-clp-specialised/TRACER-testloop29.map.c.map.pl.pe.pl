new6(A,B) :-
   -1*B>= -100,
   1*B>=51,
   1*A=0.
new5(A) :-
   -1*A>= -100,
   1*A>=51,
   1*B=0,
   new6(B,A).
new4(A) :-
   -1*A>= -49,
   1*A>=1,
   new3(A).
new4(A) :-
   -1*A>= -100,
   1*A>=51,
   new3(A).
new3(A) :-
   -1*A>= -99,
   1*A>=0,
   1*A+ -1*B= -1,
   new4(B).
new3(A) :-
   1*A=100,
   new5(A).
new2 :-
   1*A=0,
   new3(A).
new1 :-
   new2.
false :-
   new1.

