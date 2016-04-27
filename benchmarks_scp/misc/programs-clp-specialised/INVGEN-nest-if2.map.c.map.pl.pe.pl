new15(A,B,C,D,E,F) :-
   1*D>=1,
   -1*D+1*E>=1,
   1*C+ -1*F>=0,
   -1*C+1*E>=2,
   1*B=0.
new15(A,B,C,D,E,F) :-
   1*D>=1,
   1*C+ -1*F>=0,
   -1*C+1*E>=1,
   -1*D+1*E>=1,
   1*B=1,
   1*C+ -1*G= -1,
   new12(A,G,D,E,F).
new13(A,B,C,D,E) :-
   1*B+ -1*E>=0,
   1*C>=1,
   -1*C+1*D>=1,
   -1*B+1*D>=1,
   1*B>=1,
   1*F=1,
   new15(A,F,B,C,D,E).
new13(A,B,C,D,E) :-
   -1*C+1*D>=1,
   1*C>=1,
   -1*B>=0,
   1*B+ -1*E>=0,
   1*F=0,
   new15(A,F,B,C,D,E).
new12(A,B,C,D,E) :-
   1*C>=1,
   -1*C+1*D>=1,
   -1*B+1*D>=1,
   1*B+ -1*E>=0,
   new13(A,B,C,D,E).
new12(A,B,C,D,E) :-
   -1*C+1*D>=2,
   1*C>=1,
   1*B+ -1*D>=0,
   1*B+ -1*E>=0,
   1*C+ -1*F= -1,
   new7(A,B,F,D,E).
new10(A,B,C,D,E) :-
   1*C>=1,
   -1*C+1*D>=1,
   -1*B+1*D>=1,
   1*B+ -1*E>=0,
   1*B+ -1*F= -1,
   new10(A,F,C,D,E).
new10(A,B,C,D,E) :-
   1*B+ -1*D>=0,
   1*C>=1,
   -1*C+1*D>=1,
   1*B+ -1*E>=0,
   1*E+ -1*F=0,
   new12(A,F,C,D,E).
new9(A,B,C,D,E) :-
   -1*C+1*D>=1,
   -1*A>=1,
   1*C>=1,
   1*E+ -1*F=0,
   new10(A,F,C,D,E).
new9(A,B,C,D,E) :-
   1*C>=1,
   -1*C+1*D>=1,
   1*A>=1,
   1*E+ -1*F=0,
   new10(A,F,C,D,E).
new9(A,B,C,D,E) :-
   -1*C+1*D>=1,
   1*C>=1,
   1*A=0,
   1*E+ -1*F=0,
   new12(A,F,C,D,E).
new7(A,B,C,D,E) :-
   -1*C+1*D>=1,
   1*C>=1,
   new9(A,B,C,D,E).
new4(A,B,C,D,E,F) :-
   1*E>=2,
   1*B=1,
   1*G=1,
   new7(A,C,G,E,F).
new3(A,B,C,D,E) :-
   1*E>=1,
   1*D>=2,
   1*F=1,
   new4(A,F,B,C,D,E).
new2(A) :-
   1*C>=2,
   1*B>=1,
   new3(A,D,E,C,B).
new1 :-
   new2(A).
false :-
   new1.

