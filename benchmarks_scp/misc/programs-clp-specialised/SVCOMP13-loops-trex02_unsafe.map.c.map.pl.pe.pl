new7(A,B,C,D) :-
   -1*A>=1,
   1*B=0.
new6(A,B,C) :-
   -1*A>=1,
   1*D=0,
   new7(A,D,B,C).
new4(A,B,C) :-
   -1*A>=1,
   new6(A,B,C).
new3(A,B,C) :-
   -1*D>=1,
   new4(D,B,C).
new2(A) :-
   new3(A,B,C).
new1 :-
   new2(A).
false :-
   new1.

