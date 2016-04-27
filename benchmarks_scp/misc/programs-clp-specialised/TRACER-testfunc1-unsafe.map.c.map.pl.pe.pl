new8(A,B,C,D,E) :-
   -1*B+1*C>=3,
   1*B+ -1*C>= -5,
   1*A=0,
   1*D=0.
new7(A,B,C,D) :-
   -1*A+1*B>=3,
   1*A+ -1*B>= -5,
   1*C=0,
   1*E=0,
   new8(E,A,B,C,D).
new6(A,B,C,D) :-
   1*D=0,
   1*E=0,
   1*A+ -1*F= -5,
   new7(A,F,E,D).
new4(A,B,C,D) :-
   1*E=0,
   1*A+ -1*F= -3,
   new7(A,F,E,D).
new3(A,B,C,D) :-
   -1*D>=1,
   new4(A,B,C,D).
new3(A,B,C,D) :-
   1*D>=1,
   new4(A,B,C,D).
new3(A,B,C,D) :-
   1*D=0,
   new6(A,B,C,D).
new2 :-
   new3(A,B,C,D).
new1 :-
   new2.
false :-
   new1.

