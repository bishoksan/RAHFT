new31(A,B,C,D,E,F) :-
   1*B>=0,
   -1*A>= -9,
   1*A>=7,
   1*E=0,
   1*A+ -1*G= -1,
   1*B+ -1*H= -3,
   new5(G,H,C,D,E,F).
new29(A,B,C,D,E,F) :-
   1*B>=0,
   -1*A>= -7,
   1*A>=5,
   1*D=0,
   1*E=0,
   1*A+ -1*G= -2,
   1*B+ -1*H= -1,
   new5(G,H,C,D,E,F).
new27(A,B,C,D,E,F) :-
   1*B>=0,
   -1*A>= -4,
   1*A>=0,
   1*C=0,
   1*D=0,
   1*E=0,
   1*A+ -1*G= -1,
   1*B+ -1*H= -2,
   new5(G,H,C,D,E,F).
new25(A,B,C,D,E,F) :-
   1*B>=0,
   -1*A>= -7,
   1*A>=5,
   1*D=0,
   1*E=0,
   new29(A,B,C,D,E,F).
new24(A,B,C,D,E,F) :-
   1*B>=0,
   -1*C>=1,
   -1*A>= -7,
   1*A>=5,
   1*D=0,
   1*E=0,
   new25(A,B,C,D,E,F).
new24(A,B,C,D,E,F) :-
   1*B>=0,
   1*C>=1,
   -1*A>= -7,
   1*A>=5,
   1*D=0,
   1*E=0,
   new25(A,B,C,D,E,F).
new24(A,B,C,D,E,F) :-
   1*B>=0,
   -1*A>= -4,
   1*A>=0,
   1*C=0,
   1*D=0,
   1*E=0,
   new27(A,B,C,D,E,F).
new23(A,B,C,D,E,F) :-
   1*B>=0,
   -1*A>= -7,
   1*A>=0,
   1*D=0,
   1*E=0,
   new24(A,B,G,D,E,F).
new21(A,B,C,D,E,F) :-
   1*B>=0,
   -1*A>= -9,
   1*A>=7,
   1*E=0,
   new31(A,B,C,D,E,F).
new20(A,B,C,D,E,F) :-
   1*B>=0,
   -1*D>=1,
   -1*A>= -9,
   1*A>=7,
   1*E=0,
   new21(A,B,C,D,E,F).
new20(A,B,C,D,E,F) :-
   1*B>=0,
   1*D>=1,
   -1*A>= -9,
   1*A>=7,
   1*E=0,
   new21(A,B,C,D,E,F).
new20(A,B,C,D,E,F) :-
   1*B>=0,
   -1*A>= -7,
   1*A>=0,
   1*D=0,
   1*E=0,
   new23(A,B,C,D,E,F).
new19(A,B,C,D,E,F) :-
   1*B>=0,
   -1*A>= -9,
   1*A>=0,
   1*E=0,
   new20(A,B,C,G,E,F).
new17(A,B,C,D,E,F) :-
   1*B>=0,
   1*A>=9,
   1*A+ -1*G= -2,
   1*B+ -1*H= -1,
   new5(G,H,C,D,E,F).
new16(A,B,C,D,E,F) :-
   1*B>=0,
   -1*E>=1,
   1*A>=9,
   new17(A,B,C,D,E,F).
new16(A,B,C,D,E,F) :-
   1*B>=0,
   1*E>=1,
   1*A>=9,
   new17(A,B,C,D,E,F).
new16(A,B,C,D,E,F) :-
   1*B>=0,
   -1*A>= -9,
   1*A>=0,
   1*E=0,
   new19(A,B,C,D,E,F).
new14(A,B,C,D,E,F,G) :-
   1*C>=1,
   1*B>=0,
   1*A=0,
   1*G=0.
new12(A,B,C,D,E,F) :-
   -3*A+1*B>=1,
   1*A>=0,
   1*F=0,
   1*G=0,
   new14(G,A,B,C,D,E,F).
new10(A,B,C,D,E,F,G) :-
   1*C>=0,
   1*B>=1,
   1*A=0,
   1*G=0.
new10(A,B,C,D,E,F,G) :-
   -3*B+1*C>=1,
   1*B>=0,
   1*A=1,
   1*G=0,
   new12(B,C,D,E,F,G).
new9(A,B,C,D,E,F) :-
   1*B>=1,
   -1*A+2*B>=0,
   1*A>=0,
   1*F=0,
   1*G=1,
   new10(G,A,B,C,D,E,F).
new9(A,B,C,D,E,F) :-
   1*B>=0,
   1*A+ -2*B>=1,
   1*F=0,
   1*G=0,
   new10(G,A,B,C,D,E,F).
new7(A,B,C,D,E,F) :-
   1*B>=0,
   1*A>=0,
   new16(A,B,C,D,G,F).
new6(A,B,C,D,E,F) :-
   1*B>=0,
   -1*F>=1,
   1*A>=0,
   new7(A,B,C,D,E,F).
new6(A,B,C,D,E,F) :-
   1*B>=0,
   1*F>=1,
   1*A>=0,
   new7(A,B,C,D,E,F).
new6(A,B,C,D,E,F) :-
   1*A>=0,
   1*B>=0,
   1*A+1*B>=1,
   1*F=0,
   new9(A,B,C,D,E,F).
new5(A,B,C,D,E,F) :-
   1*B>=0,
   1*A>=0,
   new6(A,B,C,D,E,G).
new4(A,B,C,D,E,F) :-
   1*A=0,
   1*B=0,
   new5(A,B,C,D,E,F).
new3(A,B,C,D,E,F) :-
   1*A=0,
   1*B=0,
   new4(A,B,C,D,E,F).
new2 :-
   1*A=0,
   1*B=0,
   new3(A,B,C,D,E,F).
new1 :-
   new2.
false :-
   new1.

