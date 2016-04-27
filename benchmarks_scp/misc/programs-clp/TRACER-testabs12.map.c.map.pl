new15(A,B,C,D) :- A=0.
new12(A,B,C) :- D=0, B=< -1, new11(A,B,D).
new12(A,B,C) :- D=1, B>=0, new11(A,B,D).
new11(A,B,C) :- new15(C,A,B,C).
new10(A,B,C) :- D=0, A>=101, new11(A,B,D).
new10(A,B,C) :- A=<100, new12(A,B,C).
new7(A,B,C) :- D=1+A, E=1+B, A=<99, new7(D,E,C).
new7(A,B,C) :- A>=100, new10(A,B,C).
new6(A,B,C,D) :- new6(A,B,C,D).
new4(A,B,C,D) :- A=0, new6(A,B,C,D).
new4(A,B,C,D) :- E=0, A=< -1, new7(E,C,D).
new4(A,B,C,D) :- E=0, A>=1, new7(E,C,D).
new3(A,B,C) :- D=1, B>=0, new4(D,A,B,C).
new3(A,B,C) :- D=0, B=< -1, new4(D,A,B,C).
new2 :- new3(A,B,C).
new1 :- new2.
false :- new1.
