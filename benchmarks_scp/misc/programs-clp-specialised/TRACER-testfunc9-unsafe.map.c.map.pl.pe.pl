new15(A,B) :-
   1*A=0,
   1*B=15.
new10(A,B,C) :-
   1*B=15,
   1*D=15,
   new6(D).
new7(A,B,C) :-
   1*B=15,
   new10(A,B,C).
new6(A) :-
   1*A=15,
   1*B=0,
   new15(B,A).
new5(A,B,C) :-
   1*B=15,
   new7(A,B,C).
new4(A,B,C) :-
   1*D=15,
   new5(A,D,C).
new3(A) :-
   new4(B,C,A).
new2 :-
   new3(A).
new1 :-
   new2.
false :-
   new1.

