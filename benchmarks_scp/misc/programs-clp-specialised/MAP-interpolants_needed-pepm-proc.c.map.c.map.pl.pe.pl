new13(A,B,C,D,E) :-
   -3*A+1*B>=1,
   1*A>=4,
   1*D=0,
   1*A+ -1*F= -1,
   1*B+ -1*G= -3,
   new3(F,G,C,D,E).
new13(A,B,C,D,E) :-
   -1*A>= -3,
   -3*A+1*B>=1,
   1*A>=0,
   1*D=0,
   new3(A,B,C,D,E).
new12(A,B,C,D,E) :-
   -1*C>=1,
   -3*A+1*B>=1,
   1*A>=0,
   1*D=0,
   new13(A,B,C,D,E).
new12(A,B,C,D,E) :-
   1*C>=1,
   -3*A+1*B>=1,
   1*A>=0,
   1*D=0,
   new13(A,B,C,D,E).
new12(A,B,C,D,E) :-
   -3*A+1*B>=1,
   1*A>=0,
   1*C=0,
   1*D=0,
   new3(A,B,C,D,E).
new11(A,B,C,D,E) :-
   -3*A+1*B>=1,
   1*A>=0,
   1*D=0,
   new12(A,B,F,D,E).
new8(A,B,C,D,E) :-
   -3*A+1*B>=2,
   -1*D>=1,
   1*A>=0,
   1*A+ -1*F= -1,
   1*B+ -1*G= -2,
   new3(F,G,C,D,E).
new8(A,B,C,D,E) :-
   -3*A+1*B>=2,
   1*D>=1,
   1*A>=0,
   1*A+ -1*F= -1,
   1*B+ -1*G= -2,
   new3(F,G,C,D,E).
new8(A,B,C,D,E) :-
   -3*A+1*B>=1,
   1*A>=0,
   1*D=0,
   new11(A,B,C,D,E).
new7(A,B,C,D,E) :-
   -3*A+1*B>=1,
   1*A>=0,
   1*E=0.
new5(A,B,C,D,E) :-
   -3*A+1*B>=1,
   1*A>=0,
   new8(A,B,C,F,E).
new4(A,B,C,D,E) :-
   -1*E>=1,
   -3*A+1*B>=1,
   1*A>=0,
   new5(A,B,C,D,E).
new4(A,B,C,D,E) :-
   1*E>=1,
   -3*A+1*B>=1,
   1*A>=0,
   new5(A,B,C,D,E).
new4(A,B,C,D,E) :-
   -3*A+1*B>=1,
   1*A>=0,
   1*E=0,
   new7(A,B,C,D,E).
new3(A,B,C,D,E) :-
   -3*A+1*B>=1,
   1*A>=0,
   new4(A,B,C,D,F).

