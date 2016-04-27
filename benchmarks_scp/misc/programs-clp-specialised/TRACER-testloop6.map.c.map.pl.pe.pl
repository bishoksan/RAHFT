new15(A,B,C,D,E,F) :-
   1*C>=0,
   1*B+ -1*C>= -1,
   -1*B>= -9,
   1*B>=0,
   1*A=0,
   1*F=1.
new15(A,B,C,D,E,F) :-
   1*C>=0,
   1*B+ -1*C>= -1,
   1*B>=0,
   -1*B>= -8,
   1*A=1,
   1*F=1,
   1*B+ -1*G= -1,
   new7(G,C,D,E,F).
new13(A,B,C,D,E) :-
   1*B>=0,
   1*C>=0,
   1*A+ -1*B>= -1,
   -1*A>= -8,
   1*A>=0,
   1*E=1,
   1*F=1,
   new15(F,A,B,C,D,E).
new13(A,B,C,D,E) :-
   1*B>=0,
   1*A+ -1*B>= -1,
   -1*C>=1,
   -1*A>= -9,
   1*A>=0,
   1*E=1,
   1*F=0,
   new15(F,A,B,C,D,E).
new9(A,B,C,D,E) :-
   1*B>=0,
   1*D>=1,
   -1*A>= -9,
   1*A+ -1*B>=0,
   1*E=1,
   new13(A,B,C,D,E).
new9(A,B,C,D,E) :-
   1*B>=0,
   -1*D>=0,
   -1*A>= -9,
   1*A+ -1*B>=0,
   1*E=1,
   1*B+ -1*F= -1,
   new13(A,F,C,D,E).
new7(A,B,C,D,E) :-
   1*B>=0,
   -1*A>= -9,
   1*A+ -1*B>=0,
   1*E=1,
   new9(A,B,C,D,E).
new4(A,B,C,D,E,F) :-
   1*A=1,
   1*C=0,
   1*F=1,
   1*G=0,
   new7(G,C,D,E,F).
new3(A,B,C,D,E) :-
   1*C>=0,
   1*B=0,
   1*E=1,
   1*F=1,
   new4(F,A,B,C,D,E).
new2 :-
   1*C>=0,
   1*A=1,
   1*B=0,
   new3(D,B,C,E,A).
new1 :-
   new2.
false :-
   new1.

