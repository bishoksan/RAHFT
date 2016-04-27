new42(A,B,C,D,E) :- new4(A,B,C,D,E).
new39(A,B,C,D,E) :- F=8656, B=< -1, new42(F,B,C,D,E).
new39(A,B,C,D,E) :- F=8656, B>=1, new42(F,B,C,D,E).
new39(A,B,C,D,E) :- F=8512, B=0, new42(F,B,C,D,E).
new35(A,B,C,D,E) :- new4(A,B,C,D,E).
new34(A,B,C,D,E) :- F=8466, E=< -1, new35(F,B,C,D,E).
new34(A,B,C,D,E) :- F=8466, E>=1, new35(F,B,C,D,E).
new34(A,B,C,D,E) :- F=8640, E=0, new35(F,B,C,D,E).
new32(A,B,C,D,E) :- new4(A,B,C,D,E).
new29(A,B,C,D,E) :- F=8656, B=0, new32(F,B,C,D,E).
new26(A,B,C,D,E) :- new4(A,B,C,D,E).
new22(A,B,C,D,E) :- D=5.
new22(A,B,C,D,E) :- D=<4, new21(A,B,C,D,E).
new22(A,B,C,D,E) :- D>=6, new21(A,B,C,D,E).
new21(A,B,C,D,E) :- F=8640, B=< -1, new26(F,B,C,D,E).
new21(A,B,C,D,E) :- F=8640, B>=1, new26(F,B,C,D,E).
new18(A,B,C,D,E) :- F=5, D=4, new21(A,B,C,F,E).
new18(A,B,C,D,E) :- D=<3, new22(A,B,C,D,E).
new18(A,B,C,D,E) :- D>=5, new22(A,B,C,D,E).
new17(A,B,C,D,E) :- F=3, D=2, new18(A,B,C,F,E).
new17(A,B,C,D,E) :- D=<1, new18(A,B,C,D,E).
new17(A,B,C,D,E) :- D>=3, new18(A,B,C,D,E).
new15(A,B,C,D,E) :- A=8656, new17(A,B,C,D,E).
new14(A,B,C,D,E) :- F=4, D=3, new29(A,B,C,F,E).
new14(A,B,C,D,E) :- D=<2, new29(A,B,C,D,E).
new14(A,B,C,D,E) :- D>=4, new29(A,B,C,D,E).
new12(A,B,C,D,E) :- A=8640, new14(A,B,C,D,E).
new12(A,B,C,D,E) :- A=<8639, new15(A,B,C,D,E).
new12(A,B,C,D,E) :- A>=8641, new15(A,B,C,D,E).
new11(A,B,C,D,E) :- new34(A,B,C,D,F).
new9(A,B,C,D,E) :- A=8512, new11(A,B,C,D,E).
new9(A,B,C,D,E) :- A=<8511, new12(A,B,C,D,E).
new9(A,B,C,D,E) :- A>=8513, new12(A,B,C,D,E).
new8(A,B,C,D,E) :- F=2, D=0, new39(A,B,C,F,E).
new8(A,B,C,D,E) :- D=< -1, new39(A,B,C,D,E).
new8(A,B,C,D,E) :- D>=1, new39(A,B,C,D,E).
new7(A,B,C,D,E) :- A=8466, new8(A,B,C,D,E).
new7(A,B,C,D,E) :- A=<8465, new9(A,B,C,D,E).
new7(A,B,C,D,E) :- A>=8467, new9(A,B,C,D,E).
new6(A,B,C,D,E) :- D>=3.
new6(A,B,C,D,E) :- D=<2, new7(A,B,C,D,E).
new5(A,B,C,D,E) :- A=<8512, new6(A,B,C,D,E).
new5(A,B,C,D,E) :- A>=8513, new7(A,B,C,D,E).
new4(A,B,C,D,E) :- new5(A,B,C,D,E).
new3(A,B,C,D,E) :- F=0, G=8466, new4(G,H,H,F,E).
new2 :- new3(A,B,C,D,E).
new1 :- new2.
false :- new1.
