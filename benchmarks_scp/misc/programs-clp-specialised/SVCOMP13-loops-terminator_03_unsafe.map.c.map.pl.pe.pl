new14(A,B,C,D,E,F) :-
   1*B+ -1*C>=0,
   1*A=0,
   1*D+ -1*E=0,
   1*F=0.
new9(A,B,C,D,E) :-
   1*C>=1,
   1*A+ -1*B>=0,
   1*C+ -1*D=0,
   1*F=0,
   new8(A,B,C,D,F).
new8(A,B,C,D,E) :-
   1*A+ -1*B>=0,
   1*C+ -1*D=0,
   1*E=0,
   1*F=0,
   new14(F,A,B,C,D,E).
new7(A,B,C,D,E) :-
   1*C>=1,
   1*A+ -1*B>=0,
   1*C+ -1*D=0,
   new9(A,B,C,D,E).
new6(A,B,C,D,E) :-
   1*C>=1,
   -1*A>= -99,
   1*A+ -1*B>=0,
   1*C+ -1*D=0,
   1*A+1*C+ -1*F=0,
   new6(F,B,C,D,E).
new6(A,B,C,D,E) :-
   1*A+ -1*B>=0,
   1*C>=1,
   1*A>=100,
   1*C+ -1*D=0,
   new7(A,B,C,D,E).
new5(A,B,C,D,E) :-
   1*C>=1,
   1*A+ -1*B=0,
   1*C+ -1*D=0,
   new6(A,B,C,D,E).
new4(A,B,C,D,E) :-
   1*F>=1,
   1*A+ -1*B=0,
   1*F+ -1*G=0,
   new5(A,B,G,F,E).
new3(A,B,C,D,E) :-
   1*F+ -1*G=0,
   new4(G,F,C,D,E).
new2 :-
   new3(A,B,C,D,E).
new1 :-
   new2.
false :-
   new1.

