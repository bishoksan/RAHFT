new7(A,B,C,D,E) :-
   1*B>=0,
   1*A=0,
   1*B+ -1*C=0,
   1*D=0,
   1*B+ -1*E=0.
new6(A,B,C,D) :-
   1*A>=0,
   1*A+ -1*B=0,
   1*C=0,
   1*A+ -1*D=0,
   1*A+ -1*E=0,
   1*F=0,
   new7(F,E,B,C,D).
new4(A,B,C,D) :-
   1*A+ -1*C>=0,
   1*C>=1,
   1*A+ -1*B=0,
   1*A+ -1*C+ -1*D=0,
   1*C+ -1*E=1,
   1*A+ -1*C+ -1*F= -1,
   new4(A,B,E,F).
new4(A,B,C,D) :-
   1*A>=0,
   1*A+ -1*B=0,
   1*C=0,
   1*A+ -1*D=0,
   new6(A,B,C,D).
new3(A,B,C,D) :-
   1*E>=0,
   1*E+ -1*F=0,
   1*E+ -1*G=0,
   1*H=0,
   new4(F,G,E,H).
new2 :-
   new3(A,B,C,D).
new1 :-
   new2.
false :-
   new1.

