new98(A,B,C,D,E,F,G) :-
   1*E>=2,
   -1*C+1*F>=0,
   1*D+ -1*F>=4,
   1*C>=1,
   1*A=1,
   1*B=0,
   1*F+ -1*H= -1,
   new26(B,C,D,E,H,G).
new96(A,B,C,D,E,F) :-
   1*D>=2,
   1*C+ -1*E>=4,
   -1*B+1*E>=0,
   1*B>=1,
   1*A=0,
   1*G=1,
   new98(G,A,B,C,D,E,F).
new94(A,B,C,D,E,F,G) :-
   1*D+ -1*F>=4,
   1*E>=2,
   -1*C+1*F>=0,
   1*C>=1,
   1*A=1,
   1*B=0,
   new96(B,C,D,E,F,G).
new93(A,B,C,D,E,F) :-
   1*C+ -1*E>=4,
   1*D>=2,
   -1*B+1*E>=0,
   1*B>=1,
   1*A=0,
   1*G=1,
   new94(G,A,B,C,D,E,F).
new87(A,B,C,D,E,F,G) :-
   1*E+ -1*G>=2,
   1*G>=1,
   -1*C+1*D>=2,
   -1*C+1*F+ -1*G>= -1,
   1*C>=2,
   1*A=1,
   1*F+ -1*H= -1,
   new59(B,C,D,E,H,G).
new85(A,B,C,D,E,F) :-
   1*D+ -1*F>=2,
   1*F>=1,
   -1*B+1*C>=2,
   -1*B+1*E+ -1*F>= -1,
   1*B>=2,
   1*G=1,
   new87(G,A,B,C,D,E,F).
new83(A,B,C,D,E,F,G) :-
   1*G>=1,
   -1*C+1*D>=2,
   -1*C+1*F+ -1*G>= -1,
   1*E+ -1*G>=1,
   1*C>=2,
   -1*C+1*F>=2,
   1*A=0.
new83(A,B,C,D,E,F,G) :-
   1*G>=1,
   -1*C+1*D>=2,
   -1*C+1*F+ -1*G>= -1,
   1*C>=2,
   1*E+ -1*G>=2,
   1*A=1,
   new85(B,C,D,E,F,G).
new81(A,B,C,D,E,F) :-
   1*F>=1,
   -1*B+1*C>=2,
   1*D+ -1*F>=2,
   1*C+ -1*E>=1,
   -1*B+1*E+ -1*F>= -1,
   1*B>=2,
   1*G=1,
   new83(G,A,B,C,D,E,F).
new81(A,B,C,D,E,F) :-
   -1*C+1*E>=0,
   -1*B+1*C>=2,
   1*D+ -1*F>=1,
   1*F>=1,
   -1*B+1*E+ -1*F>= -1,
   1*B>=2,
   1*G=0,
   new83(G,A,B,C,D,E,F).
new79(A,B,C,D,E,F,G) :-
   1*E+ -1*G>=1,
   1*G>=1,
   -1*C+1*D>=2,
   -1*C+2*E+1*F+ -2*G>=4,
   -1*C+1*F+ -1*G>= -1,
   1*C>=2,
   1*A=1,
   new81(B,C,D,E,F,G).
new77(A,B,C,D,E,F) :-
   1*D+ -1*F>=1,
   1*F>=1,
   -1*B+1*C>=2,
   -1*B+2*D+1*E+ -2*F>=4,
   -1*B+1*E+ -1*F>= -1,
   1*B>=2,
   1*G=1,
   new79(G,A,B,C,D,E,F).
new75(A,B,C,D,E,F,G) :-
   1*E+ -1*G>=1,
   1*G>=1,
   -1*C+1*D>=2,
   -1*C+2*E+1*F+ -2*G>=4,
   -1*C+1*F+ -1*G>= -1,
   1*C>=2,
   1*A=1,
   new77(B,C,D,E,F,G).
new72(A,B,C,D,E,F) :-
   1*D+ -1*F>=1,
   1*F>=1,
   -1*B+1*C>=2,
   -1*B+2*D+1*E+ -2*F>=4,
   -1*B+1*E+ -1*F>= -1,
   1*B>=2,
   1*G=1,
   new75(G,A,B,C,D,E,F).
new70(A,B,C,D,E,F) :-
   1*D+ -1*F>=2,
   1*F>=0,
   -1*B+1*C>=2,
   -1*B+2*D+1*E+ -2*F>=6,
   -1*B+1*E+ -1*F>=0,
   -1*A>=1,
   1*B>=2,
   1*F+ -1*G= -1,
   new72(A,B,C,D,E,G).
new70(A,B,C,D,E,F) :-
   1*B>=2,
   1*D+ -1*F>=2,
   1*F>=0,
   -1*B+1*C>=2,
   -1*B+2*D+1*E+ -2*F>=6,
   -1*B+1*E+ -1*F>=0,
   1*A>=1,
   1*F+ -1*G= -1,
   new72(A,B,C,D,E,G).
new70(A,B,C,D,E,F) :-
   1*F>=0,
   -1*B+1*C>=2,
   -1*B+1*E+ -1*F>=0,
   1*D+ -1*F>=2,
   1*B>=2,
   1*A=0,
   1*E+ -1*G= -1,
   new59(A,B,C,D,G,F).
new68(A,B,C,D,E,F,G) :-
   1*E+ -1*G>=2,
   1*G>=0,
   -1*C+1*D>=2,
   -1*C+1*F+ -1*G>=0,
   1*C>=2,
   1*A=1,
   new70(B,C,D,E,F,G).
new66(A,B,C,D,E,F) :-
   1*D+ -1*F>=2,
   1*F>=0,
   -1*B+1*C>=2,
   -1*B+1*E+ -1*F>=0,
   1*B>=2,
   1*G=1,
   new68(G,A,B,C,D,E,F).
new64(A,B,C,D,E,F,G) :-
   1*E+ -1*G>=2,
   1*G>=0,
   -1*C+1*D>=2,
   -1*C+1*F+ -1*G>=0,
   1*C>=2,
   -1*C+1*F>=2,
   1*A=0.
new64(A,B,C,D,E,F,G) :-
   1*E+ -1*G>=2,
   1*G>=0,
   -1*C+1*D>=2,
   1*C>=2,
   -1*C+1*F+ -1*G>=0,
   1*A=1,
   new66(B,C,D,E,F,G).
new63(A,B,C,D,E,F) :-
   1*D+ -1*F>=2,
   1*F>=0,
   -1*B+1*C>=2,
   1*C+ -1*E>=1,
   -1*B+1*E+ -1*F>=0,
   1*B>=2,
   1*G=1,
   new64(G,A,B,C,D,E,F).
new63(A,B,C,D,E,F) :-
   1*F>=0,
   -1*C+1*E>=0,
   -1*B+1*C>=2,
   1*D+ -1*F>=2,
   -1*B+1*E+ -1*F>=0,
   1*B>=2,
   1*G=0,
   new64(G,A,B,C,D,E,F).
new61(A,B,C,D,E,F) :-
   1*D+ -1*F>=2,
   1*F>=0,
   -1*B+1*C>=2,
   -1*B+1*E+ -1*F>=0,
   1*B>=2,
   new63(A,B,C,D,E,F).
new59(A,B,C,D,E,F) :-
   1*D+ -1*F>=2,
   1*F>=0,
   1*C+ -1*E>=2,
   -1*B+1*E+ -1*F>=0,
   1*B>=2,
   new61(A,B,C,D,E,F).
new59(A,B,C,D,E,F) :-
   1*D+ -1*F>=2,
   1*F>=0,
   -1*C+1*E>=0,
   -1*B+1*C>=2,
   -1*B+1*E+ -1*F>=0,
   1*B>=2,
   new61(A,B,C,D,E,F).
new57(A,B,C,D,E,F,G) :-
   1*E>=2,
   -1*C+1*D>=2,
   1*C>=2,
   1*A=1,
   1*C+ -1*F=0,
   1*G=0,
   new59(B,C,D,E,F,G).
new55(A,B,C,D,E,F) :-
   1*D>=2,
   -1*B+1*C>=2,
   1*B>=2,
   1*B+ -1*E=0,
   1*F=0,
   1*G=1,
   new57(G,A,B,C,D,E,F).
new53(A,B,C,D,E,F,G) :-
   1*E>=2,
   -1*C+1*D>=2,
   1*C>=2,
   1*A=1,
   1*C+ -1*F=0,
   1*G=0,
   new55(B,C,D,E,F,G).
new51(A,B,C,D,E,F) :-
   1*D>=2,
   -1*B+1*C>=2,
   1*B>=2,
   1*B+ -1*E=0,
   1*F=0,
   1*G=1,
   new53(G,A,B,C,D,E,F).
new49(A,B,C,D,E,F) :-
   1*B>=2,
   -1*B+1*C>=2,
   -1*A>=1,
   1*D>=2,
   1*B+ -1*E=0,
   1*G=0,
   new51(A,B,C,D,E,G).
new49(A,B,C,D,E,F) :-
   1*D>=2,
   1*B>=2,
   -1*B+1*C>=2,
   1*A>=1,
   1*B+ -1*E=0,
   1*G=0,
   new51(A,B,C,D,E,G).
new47(A,B,C,D,E,F) :-
   1*D>=2,
   -1*B+1*E>=0,
   1*C+ -1*E>=3,
   1*B>=1,
   1*E+ -1*G= -1,
   1*E+ -1*H= -1,
   new49(A,G,C,D,H,F).
new45(A,B,C,D,E,F,G) :-
   1*D+ -1*F>=3,
   1*E>=2,
   -1*C+1*F>=0,
   1*C>=1,
   1*A=1,
   new47(B,C,D,E,F,G).
new43(A,B,C,D,E,F) :-
   1*D>=2,
   1*C+ -1*E>=3,
   -1*B+1*E>=0,
   1*B>=1,
   1*G=1,
   new45(G,A,B,C,D,E,F).
new41(A,B,C,D,E,F,G) :-
   1*D+ -1*F>=3,
   1*E>=2,
   -1*C+1*F>=0,
   1*C>=1,
   1*A=1,
   new43(B,C,D,E,F,G).
new39(A,B,C,D,E,F) :-
   1*C+ -1*E>=3,
   1*D>=2,
   -1*B+1*E>=0,
   1*B>=1,
   1*G=1,
   new41(G,A,B,C,D,E,F).
new37(A,B,C,D,E,F) :-
   1*C+ -1*E>=3,
   1*D>=2,
   -1*B+1*E>=0,
   1*B>=1,
   new39(A,B,C,D,E,F).
new35(A,B,C,D,E,F,G) :-
   1*D+ -1*F>=3,
   1*E>=2,
   -1*C+1*F>=0,
   1*C>=1,
   1*A=1,
   new37(B,C,D,E,F,G).
new33(A,B,C,D,E,F) :-
   1*D>=2,
   1*C+ -1*E>=3,
   -1*B+1*E>=0,
   1*B>=1,
   1*G=1,
   new35(G,A,B,C,D,E,F).
new31(A,B,C,D,E,F,G) :-
   1*D+ -1*F>=3,
   1*E>=2,
   -1*C+1*F>=0,
   1*C>=1,
   1*A=1,
   new33(B,C,D,E,F,G).
new30(A,B,C,D,E,F) :-
   1*C+ -1*E>=3,
   1*D>=2,
   -1*B+1*E>=0,
   1*B>=1,
   1*G=1,
   new31(G,A,B,C,D,E,F).
new28(A,B,C,D,E,F) :-
   1*C+ -1*E>=3,
   1*D>=2,
   -1*B+1*E>=0,
   -1*A>=1,
   1*B>=1,
   new30(A,B,C,D,E,F).
new28(A,B,C,D,E,F) :-
   1*B>=1,
   1*C+ -1*E>=3,
   1*D>=2,
   -1*B+1*E>=0,
   1*A>=1,
   new30(A,B,C,D,E,F).
new28(A,B,C,D,E,F) :-
   1*C+ -1*E>=4,
   1*D>=2,
   -1*B+1*E>=0,
   1*B>=1,
   1*A=0,
   new93(A,B,C,D,E,F).
new26(A,B,C,D,E,F) :-
   1*C+ -1*E>=3,
   1*D>=2,
   -1*B+1*E>=0,
   1*B>=1,
   new28(A,B,C,D,E,F).
new24(A,B,C,D,E,F,G) :-
   1*E>=2,
   -1*C+1*D>=3,
   1*C>=1,
   1*A=1,
   1*C+ -1*F=0,
   new26(B,C,D,E,F,G).
new22(A,B,C,D,E,F) :-
   1*D>=2,
   -1*B+1*C>=3,
   1*B>=1,
   1*B+ -1*E=0,
   1*G=1,
   new24(G,A,B,C,D,E,F).
new20(A,B,C,D,E,F,G) :-
   1*E>=2,
   -1*C+1*D>=3,
   1*C>=1,
   1*A=1,
   1*C+ -1*F=0,
   new22(B,C,D,E,F,G).
new18(A,B,C,D,E,F) :-
   1*D>=2,
   -1*B+1*C>=3,
   1*B>=1,
   1*B+ -1*E=0,
   1*G=1,
   new20(G,A,B,C,D,E,F).
new16(A,B,C,D,E,F) :-
   1*D>=2,
   -1*B+1*C>=3,
   -1*A>=1,
   1*B>=1,
   1*B+ -1*E=0,
   new18(A,B,C,D,E,F).
new16(A,B,C,D,E,F) :-
   1*B>=1,
   1*D>=2,
   -1*B+1*C>=3,
   1*A>=1,
   1*B+ -1*E=0,
   new18(A,B,C,D,E,F).
new14(A,B,C,D,E,F,G) :-
   1*E>=2,
   -1*C+1*D>=3,
   1*C>=1,
   1*A=1,
   1*C+ -1*F=0,
   new16(B,C,D,E,F,G).
new12(A,B,C,D,E,F) :-
   1*B>=1,
   -1*B+1*C>=3,
   1*D>=2,
   1*B+ -1*E=0,
   1*G=1,
   new14(G,A,B,C,D,E,F).
new10(A,B,C,D,E,F,G) :-
   1*E>=2,
   -1*C+1*D>=3,
   1*C>=1,
   1*A=1,
   1*C+ -1*F=0,
   new12(B,C,D,E,F,G).
new9(A,B,C,D,E,F) :-
   1*D>=2,
   -1*B+1*C>=3,
   1*B>=1,
   1*B+ -1*E=0,
   1*G=1,
   new10(G,A,B,C,D,E,F).
new7(A,B,C,D,E,F) :-
   1*B>=1,
   -1*B+1*C>=3,
   1*D>=2,
   1*B+ -1*G=0,
   new9(A,G,C,D,B,F).
new6(A,B,C,D,E,F) :-
   1*D>=2,
   -1*B+1*C>=3,
   1*B>=1,
   new7(A,B,C,D,E,F).
new5(A,B,C,D,E,F) :-
   1*D>=2,
   -1*B+1*C>=3,
   1*B>=1,
   new6(A,B,C,D,E,F).
new4(A,B,C,D,E,F) :-
   1*D>=2,
   -1*B+1*C>=3,
   1*B>=1,
   new5(A,B,C,D,E,F).
new3(A,B,C,D,E,F) :-
   1*D>=2,
   -1*B+1*C>=3,
   1*B>=1,
   new4(A,B,C,D,E,F).
new2 :-
   1*B>=1,
   -1*B+1*C>=3,
   1*A>=2,
   new3(D,B,C,A,E,F).
new1 :-
   new2.
false :-
   new1.

