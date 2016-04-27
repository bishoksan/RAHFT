new25(A,B,C,D,E) :- D=< -1.
new25(A,B,C,D,E) :- F=1+C, D>=0, new9(A,B,F,D,E).
new24(A,B,C,D,E) :- D-A>=0.
new24(A,B,C,D,E) :- A-D>=1, new25(A,B,C,D,E).
new23(A,B,C,D,E) :- B=< -1.
new23(A,B,C,D,E) :- B>=0, new24(A,B,C,D,E).
new20(A,B,C,D,E) :- B-A>=0.
new20(A,B,C,D,E) :- A-B>=1, new23(A,B,C,D,E).
new17(A,B,C,D,E) :- B-D=< -1, new20(A,B,C,D,E).
new17(A,B,C,D,E) :- B-D>=1, new20(A,B,C,D,E).
new17(A,B,C,B,D) :- E=1+C, new9(A,B,E,B,D).
new16(A,B,C,D,E) :- E=< -1, new17(A,B,C,C,E).
new16(A,B,C,D,E) :- E>=1, new17(A,B,C,C,E).
new16(A,B,C,D,E) :- E=0, new17(A,B,C,D,E).
new15(A,B,C,D,E) :- new16(A,B,C,D,F).
new14(A,B,C,D,E) :- D=< -1.
new14(A,B,C,D,E) :- D>=0, new15(A,B,C,D,E).
new13(A,B,C,D,E) :- D-A>=0.
new13(A,B,C,D,E) :- A-D>=1, new14(A,B,C,D,E).
new12(A,B,C,D,E) :- C=< -1.
new12(A,B,C,D,E) :- C>=0, new13(A,B,C,D,E).
new10(A,B,C,D,E) :- C-A>=0.
new10(A,B,C,D,E) :- A-C>=1, new12(A,B,C,D,E).
new9(A,B,C,D,E) :- A-C>=1, new10(A,B,C,D,E).
new9(A,B,C,D,E) :- F=1+B, A-C=<0, new7(A,F,C,D,E).
new7(A,B,C,D,E) :- F=1+B, A-B>=2, new9(A,B,F,B,E).
new6(A,B,C,D,E,F) :- new6(A,B,C,D,E,F).
new4(A,B,C,D,E,F) :- A=0, new6(A,B,C,D,E,F).
new4(A,B,C,D,E,F) :- A=< -1, new7(B,C,D,E,F).
new4(A,B,C,D,E,F) :- A>=1, new7(B,C,D,E,F).
new3(A,B,C,D,E) :- F=1, A>=0, new4(F,A,B,C,D,E).
new3(A,B,C,D,E) :- F=0, A=< -1, new4(F,A,B,C,D,E).
new2 :- A=0, B=0, C=0, new3(D,B,C,A,E).
new1 :- new2.
false :- new1.
