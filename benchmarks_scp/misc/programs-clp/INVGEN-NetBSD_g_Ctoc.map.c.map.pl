new28(A,B,C,D,E) :- F= -1+E, G=1+C, H=1+D, B=0, new9(A,B,G,H,F).
new26(A,B,C,D,E,F) :- C=0.
new26(A,B,C,D,E,F) :- C=< -1, new28(A,B,D,E,F).
new26(A,B,C,D,E,F) :- C>=1, new28(A,B,D,E,F).
new24(A,B,C,D,E) :- F=1, A-C>=1, new26(A,B,F,C,D,E).
new24(A,B,C,D,E) :- F=0, A-C=<0, new26(A,B,F,C,D,E).
new22(A,B,C,D,E,F) :- C=0.
new22(A,B,C,D,E,F) :- C=< -1, new24(A,B,D,E,F).
new22(A,B,C,D,E,F) :- C>=1, new24(A,B,D,E,F).
new20(A,B,C,D,E) :- F=1, C>=0, new22(A,B,F,C,D,E).
new20(A,B,C,D,E) :- F=0, C=< -1, new22(A,B,F,C,D,E).
new18(A,B,C,D,E,F) :- C=0.
new18(A,B,C,D,E,F) :- C=< -1, new20(A,B,D,E,F).
new18(A,B,C,D,E,F) :- C>=1, new20(A,B,D,E,F).
new16(A,B,C,D,E) :- F=1, A-D>=1, new18(A,B,F,C,D,E).
new16(A,B,C,D,E) :- F=0, A-D=<0, new18(A,B,F,C,D,E).
new14(A,B,C,D,E,F) :- C=0.
new14(A,B,C,D,E,F) :- C=< -1, new16(A,B,D,E,F).
new14(A,B,C,D,E,F) :- C>=1, new16(A,B,D,E,F).
new12(A,B,C,D,E) :- F=1, D>=0, new14(A,B,F,C,D,E).
new12(A,B,C,D,E) :- F=0, D=< -1, new14(A,B,F,C,D,E).
new11(A,B,C,D,E) :- E=< -1, new12(A,B,C,D,E).
new11(A,B,C,D,E) :- E>=1, new12(A,B,C,D,E).
new9(A,B,C,D,E) :- new11(A,B,C,D,E).
new7(A,B,C,D,E) :- F=0, G=0, E=< -1, new9(A,B,G,F,E).
new7(A,B,C,D,E) :- F=0, G=0, E>=1, new9(A,B,G,F,E).
new5(A,B,C,D,E,F) :- C=0.
new5(A,B,C,D,E,F) :- C=< -1, new7(A,B,D,E,F).
new5(A,B,C,D,E,F) :- C>=1, new7(A,B,D,E,F).
new4(A,B,C,D,E) :- F=1, A>=1, new5(A,B,F,C,D,E).
new4(A,B,C,D,E) :- F=0, A=<0, new5(A,B,F,C,D,E).
new3(A,B,C,D,E) :- A>=1, new4(A,B,C,D,E).
new2(A,B) :- new3(A,B,C,D,A).
new1 :- new2(A,B).
false :- new1.
