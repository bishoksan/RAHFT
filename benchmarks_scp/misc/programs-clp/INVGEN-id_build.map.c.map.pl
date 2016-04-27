new11(A,B,C,D) :- A=0.
new11(A,B,C,D) :- E=1+D, A=< -1, new4(B,C,E).
new11(A,B,C,D) :- E=1+D, A>=1, new4(B,C,E).
new9(A,B,C) :- D=1, B>=0, new11(D,A,B,C).
new9(A,B,C) :- D=0, B=< -1, new11(D,A,B,C).
new7(A,B,C,D) :- A=0.
new7(A,B,C,D) :- A=< -1, new9(B,C,D).
new7(A,B,C,D) :- A>=1, new9(B,C,D).
new5(A,B,C) :- D=1, A-B>=1, new7(D,A,B,C).
new5(A,B,C) :- D=0, A-B=<0, new7(D,A,B,C).
new4(A,B,C) :- C=<7, new5(A,B,C).
new4(A,B,C) :- D=1+B, C>=8, new3(A,D,C).
new3(A,B,C) :- D=0, A-B>=1, new4(A,B,D).
new2 :- A=0, new3(B,A,C).
new1 :- new2.
false :- new1.
