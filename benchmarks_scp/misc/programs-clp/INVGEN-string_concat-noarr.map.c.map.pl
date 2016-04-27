new14(A,B,C,D) :- B=0.
new13(A,B,C) :- D=1, B=<199, new14(A,D,B,C).
new13(A,B,C) :- D=0, B>=200, new14(A,D,B,C).
new12(A,B,C) :- new7(A,B,C).
new11(A,B,C) :- C>=100, new12(A,B,C).
new11(A,B,C) :- C=<99, new13(A,B,C).
new8(A,B,C) :- D=1+B, E=1+C, A=< -1, new8(A,D,E).
new8(A,B,C) :- D=1+B, E=1+C, A>=1, new8(A,D,E).
new8(A,B,C) :- A=0, new11(A,B,C).
new7(A,B,C) :- new7(A,B,C).
new6(A,B,C) :- B>=100, new7(A,B,C).
new6(A,B,C) :- D=0, B=<99, new8(A,B,D).
new3(A,B,C) :- D=1+B, A=< -1, new3(A,D,C).
new3(A,B,C) :- D=1+B, A>=1, new3(A,D,C).
new3(A,B,C) :- A=0, new6(A,B,C).
new2(A) :- B=0, new3(A,B,C).
new1 :- new2(A).
false :- new1.
