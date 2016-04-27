new7(A,B) :-
   1*A=1,
   1*B=0.
new6(A) :-
   1*A=1,
   1*B=0,
   new7(A,B).
new5(A) :-
   1*A=0,
   1*B=1,
   new6(B).
new4(A) :-
   1*A=0,
   new5(A).
new3(A) :-
   1*A=0,
   new4(A).
new2(A) :-
   1*A=0,
   new3(A).
new1 :-
   1*A=0,
   new2(A).
false :-
   new1.

