new11(A,B,C,D) :- A=0.
new11(A,B,C,D) :- E=1+B, A=< -1, new5(E,C,D).
new11(A,B,C,D) :- E=1+B, A>=1, new5(E,C,D).
new9(A,B,C,D) :- A=0.
new8(A,B,C) :- D=1, B>=0, new9(D,A,B,C).
new8(A,B,C) :- D=0, B=< -1, new9(D,A,B,C).
new7(A,B,C) :- D=1, C=<2, new11(D,A,B,C).
new7(A,B,C) :- D=0, C>=3, new11(D,A,B,C).
new5(A,B,C) :- A=<9, new7(A,B,C).
new5(A,B,C) :- A>=10, new8(A,B,C).
new4(A,B,C) :- D=0, E=0, B=< -1, new5(D,E,C).
new4(A,B,C) :- D=0, B>=0, new5(D,B,C).
new3(A,B,C) :- C=<2, new4(A,B,C).
new2 :- new3(A,B,C).
new1 :- new2.
false :- new1.
