new12(A,B,C,D,E,F,G,H) :-
   -1*G>=1,
   -1*E+1*F>=0,
   1*E>=101,
   -1*A>= -99,
   1*A+ -1*B+ -1*E+1*F>=0,
   1*C+ -1*D=0,
   1*G+ -1*H=0,
   1*A+ -1*I= -1,
   new6(I,B,C,D,E,F,G,H).
new12(A,B,C,D,E,F,G,H) :-
   1*G>=1,
   -1*E+1*F>=0,
   1*E>=101,
   -1*A>= -99,
   1*A+ -1*B+ -1*E+1*F>=0,
   1*C+ -1*D=0,
   1*G+ -1*H=0,
   1*A+ -1*I= -1,
   new6(I,B,C,D,E,F,G,H).
new12(A,B,C,D,E,F,G,H) :-
   -1*E+1*F>=0,
   1*A+ -1*B+ -1*E+1*F>=0,
   -1*A>= -99,
   1*E>=101,
   1*C+ -1*D=0,
   1*G=0,
   1*H=0,
   1*A+ -1*I=1,
   1*E+ -1*J=1,
   new6(I,B,C,D,J,F,G,H).
new10(A,B,C,D,E,F,G,H) :-
   1*E>=101,
   -1*E+1*F>=0,
   -1*A>= -99,
   1*A+ -1*B+ -1*E+1*F>=0,
   1*C+ -1*D=0,
   1*I+ -1*J=0,
   new12(A,B,C,D,E,F,J,I).
new9(A,B,C,D,E,F,G,H,I) :-
   -1*F+1*G>=0,
   1*B+ -1*C+ -1*F+1*G>=0,
   1*A=0,
   1*D+ -1*E=0.
new8(A,B,C,D,E,F,G,H) :-
   -1*E+1*F>=0,
   1*A+ -1*B+ -1*E+1*F>=0,
   1*C+ -1*D=0,
   1*I=0,
   new9(I,A,B,C,D,E,F,G,H).
new7(A,B,C,D,E,F,G,H) :-
   1*E>=101,
   -1*E+1*F>=0,
   -1*A>= -99,
   1*A+ -1*B+ -1*E+1*F>=0,
   1*C+ -1*D=0,
   new10(A,B,C,D,E,F,G,H).
new7(A,B,C,D,E,F,G,H) :-
   -1*E+1*F>=0,
   -1*E>= -100,
   -1*A>= -99,
   1*A+ -1*B+ -1*E+1*F>=0,
   1*C+ -1*D=0,
   new8(A,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :-
   -1*E+1*F>=0,
   -1*A>= -99,
   1*A+ -1*B+ -1*E+1*F>=0,
   1*C+ -1*D=0,
   new7(A,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :-
   1*A+ -1*B+ -1*E+1*F>=0,
   -1*E+1*F>=0,
   1*A>=100,
   1*C+ -1*D=0,
   new8(A,B,C,D,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :-
   1*A+ -1*B=0,
   1*C+ -1*D=0,
   1*I+ -1*J=0,
   new6(A,B,C,D,J,I,G,H).
new4(A,B,C,D,E,F,G,H) :-
   1*A+ -1*B=0,
   1*I+ -1*J=0,
   new5(A,B,J,I,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :-
   1*I+ -1*J=0,
   new4(J,I,C,D,E,F,G,H).
new2 :-
   new3(A,B,C,D,E,F,G,H).
new1 :-
   new2.
false :-
   new1.

