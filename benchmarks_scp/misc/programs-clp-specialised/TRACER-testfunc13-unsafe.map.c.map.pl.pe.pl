new12(A,B,C) :-
   1*A=0,
   1*C=2.
new11(A,B) :-
   1*B=2,
   1*C=0,
   new12(C,A,B).
new7(A,B,C,D,E,F) :-
   1*D>=4,
   -1*D>= -5,
   1*A>=0,
   1*B=0,
   1*F=2,
   new11(E,F).
new6(A,B,C,D,E,F) :-
   1*C>=1,
   1*A=0,
   1*B=0,
   1*F=2,
   1*G=4,
   new7(A,B,C,G,E,F).
new6(A,B,C,D,E,F) :-
   -1*C>=0,
   1*A=0,
   1*B=0,
   1*F=2,
   1*G=5,
   new7(A,B,C,G,E,F).
new4(A,B) :-
   1*B=2,
   1*C=0,
   1*D=0,
   new6(C,D,E,F,A,B).
new3(A,B) :-
   -1*A>=0,
   1*C=2,
   new4(A,C).
new2 :-
   -1*A>=0,
   new3(A,B).
new1 :-
   new2.
false :-
   new1.

