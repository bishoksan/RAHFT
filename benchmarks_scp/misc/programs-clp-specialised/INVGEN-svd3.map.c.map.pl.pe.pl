new23(A,B,C,D,E,F,G) :-
   1*D>=1,
   1*E+ -1*G>=0,
   1*C+ -1*D>=1,
   1*C+ -1*E>=2,
   1*B=0.
new23(A,B,C,D,E,F,G) :-
   1*D>=1,
   1*E+ -1*G>=0,
   1*C+ -1*D>=1,
   1*C+ -1*E>=0,
   1*B=1,
   1*E+ -1*H= -1,
   new12(A,C,D,H,F,G).
new19(A,B,C,D,E,F) :-
   1*B+ -1*C>=1,
   1*C>=2,
   1*D+ -1*F>=0,
   1*E+ -1*F>=0,
   1*B+ -1*D>=0,
   1*B+ -1*E>=0,
   1*E+ -1*G= -1,
   new19(A,B,C,D,G,F).
new19(A,B,C,D,E,F) :-
   1*D+ -1*F>=0,
   1*C>=2,
   1*B+ -1*D>=0,
   1*B+ -1*C>=1,
   1*B+ -1*E= -1,
   1*D+ -1*G= -1,
   new18(A,B,C,G,E,F).
new18(A,B,C,D,E,F) :-
   1*B+ -1*C>=1,
   1*C>=2,
   1*D+ -1*F>=0,
   1*B+ -1*D>=0,
   1*F+ -1*G=0,
   new19(A,B,C,D,G,F).
new18(A,B,C,D,E,F) :-
   1*C>=2,
   1*D+ -1*F>=0,
   -1*B+1*D>=1,
   1*B+ -1*C>=1,
   1*F+ -1*G=0,
   new14(A,B,C,G,E,F).
new17(A,B,C,D,E,F) :-
   1*B+ -1*C>=1,
   1*C>=1,
   1*D>=1,
   1*D+ -1*F>=0,
   1*B+ -1*D>=0,
   1*G=1,
   new23(A,G,B,C,D,E,F).
new17(A,B,C,D,E,F) :-
   1*D+ -1*F>=0,
   1*C>=1,
   -1*D>=0,
   1*B+ -1*C>=1,
   1*G=0,
   new23(A,G,B,C,D,E,F).
new14(A,B,C,D,E,F) :-
   1*C>=2,
   1*D+ -1*F>=0,
   1*B+ -1*C>=1,
   1*B+ -1*D>=0,
   1*D+ -1*G= -1,
   new14(A,B,C,G,E,F).
new14(A,B,C,D,E,F) :-
   1*D+ -1*F>=0,
   1*C>=2,
   -1*B+1*D>=1,
   1*B+ -1*C>=1,
   1*C+ -1*G=1,
   new7(A,B,G,D,E,C).
new12(A,B,C,D,E,F) :-
   1*B+ -1*C>=1,
   1*C>=1,
   1*D+ -1*F>=0,
   1*B+ -1*D>=0,
   new17(A,B,C,D,E,F).
new12(A,B,C,D,E,F) :-
   1*C>=2,
   1*D+ -1*F>=0,
   -1*B+1*D>=1,
   1*B+ -1*C>=1,
   1*F+ -1*G=0,
   new18(A,B,C,G,E,F).
new10(A,B,C,D,E,F) :-
   1*C>=1,
   -1*A>=1,
   1*B+ -1*C>=1,
   1*F+ -1*G=0,
   new12(A,B,C,G,E,F).
new10(A,B,C,D,E,F) :-
   1*B+ -1*C>=1,
   1*C>=1,
   1*A>=1,
   1*F+ -1*G=0,
   new12(A,B,C,G,E,F).
new10(A,B,C,D,E,F) :-
   1*C>=2,
   1*B+ -1*C>=1,
   1*A=0,
   1*F+ -1*G=0,
   new14(A,B,C,G,E,F).
new9(A,B,C,D,E,F) :-
   1*C>=1,
   1*B+ -1*C>=1,
   new10(A,B,C,D,E,F).
new9(A,B,C,D,E,F) :-
   1*B>=2,
   1*B+ -1*C=0,
   1*B+ -1*G=1,
   new7(A,B,G,D,E,C).
new7(A,B,C,D,E,F) :-
   1*B+ -1*C>=0,
   1*C>=1,
   1*B>=2,
   new9(A,B,C,D,E,F).
new4(A,B,C,D,E,F,G) :-
   1*C>=2,
   1*B=1,
   1*C+ -1*H=0,
   new7(A,H,C,E,F,G).
new3(A,B,C,D,E,F) :-
   1*F>=2,
   1*B>=2,
   1*G=1,
   new4(A,G,B,C,D,E,F).
new2(A) :-
   1*C>=2,
   1*B>=2,
   new3(A,C,D,E,F,B).
new1 :-
   new2(A).
false :-
   new1.

