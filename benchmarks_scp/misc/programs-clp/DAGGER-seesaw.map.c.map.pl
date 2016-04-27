new31(A,B,C,D,E,F) :- G=1+A, H=3+B, A=<9, new5(G,H,C,D,E,F).
new29(A,B,C,D,E,F) :- G=2+A, H=1+B, A=<7, new5(G,H,C,D,E,F).
new27(A,B,C,D,E,F) :- G=1+A, H=2+B, A=<4, new5(G,H,C,D,E,F).
new25(A,B,C,D,E,F) :- A>=5, new29(A,B,C,D,E,F).
new24(A,B,C,D,E,F) :- C=< -1, new25(A,B,C,D,E,F).
new24(A,B,C,D,E,F) :- C>=1, new25(A,B,C,D,E,F).
new24(A,B,C,D,E,F) :- C=0, new27(A,B,C,D,E,F).
new23(A,B,C,D,E,F) :- new24(A,B,G,D,E,F).
new21(A,B,C,D,E,F) :- A>=7, new31(A,B,C,D,E,F).
new20(A,B,C,D,E,F) :- D=< -1, new21(A,B,C,D,E,F).
new20(A,B,C,D,E,F) :- D>=1, new21(A,B,C,D,E,F).
new20(A,B,C,D,E,F) :- D=0, new23(A,B,C,D,E,F).
new19(A,B,C,D,E,F) :- new20(A,B,C,G,E,F).
new17(A,B,C,D,E,F) :- G=2+A, H=1+B, A>=9, new5(G,H,C,D,E,F).
new16(A,B,C,D,E,F) :- E=< -1, new17(A,B,C,D,E,F).
new16(A,B,C,D,E,F) :- E>=1, new17(A,B,C,D,E,F).
new16(A,B,C,D,E,F) :- E=0, new19(A,B,C,D,E,F).
new14(A,B,C,D,E,F,G) :- A=0.
new12(A,B,C,D,E,F) :- G=1, 3*A-1*B>=0, new14(G,A,B,C,D,E,F).
new12(A,B,C,D,E,F) :- G=0, 3*A-1*B=< -1, new14(G,A,B,C,D,E,F).
new10(A,B,C,D,E,F,G) :- A=0.
new10(A,B,C,D,E,F,G) :- A=< -1, new12(B,C,D,E,F,G).
new10(A,B,C,D,E,F,G) :- A>=1, new12(B,C,D,E,F,G).
new9(A,B,C,D,E,F) :- G=1, A-2*B=<0, new10(G,A,B,C,D,E,F).
new9(A,B,C,D,E,F) :- G=0, A-2*B>=1, new10(G,A,B,C,D,E,F).
new7(A,B,C,D,E,F) :- new16(A,B,C,D,G,F).
new6(A,B,C,D,E,F) :- F=< -1, new7(A,B,C,D,E,F).
new6(A,B,C,D,E,F) :- F>=1, new7(A,B,C,D,E,F).
new6(A,B,C,D,E,F) :- F=0, new9(A,B,C,D,E,F).
new5(A,B,C,D,E,F) :- new6(A,B,C,D,E,G).
new4(A,B,C,D,E,F) :- B=0, new5(A,B,C,D,E,F).
new3(A,B,C,D,E,F) :- A=0, new4(A,B,C,D,E,F).
new2 :- new3(A,B,C,D,E,F).
new1 :- new2.
false :- new1.
