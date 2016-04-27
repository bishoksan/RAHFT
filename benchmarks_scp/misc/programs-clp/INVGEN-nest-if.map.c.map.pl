new11(A,B,C,D) :- A=0.
new11(A,B,C,D) :- E=1+B, A=< -1, new4(E,C,D).
new11(A,B,C,D) :- E=1+B, A>=1, new4(E,C,D).
new7(A,B,C) :- D=1+A, A-C=< -1, new7(D,B,C).
new7(A,B,C) :- D=1+B, A-C>=0, new3(A,D,C).
new6(A,B,C) :- D=1, A-C=< -1, new7(D,B,C).
new6(A,B,C) :- D=1+B, A-C>=0, new3(A,D,C).
new5(A,B,C) :- D=1, B>=1, new11(D,A,B,C).
new5(A,B,C) :- D=0, B=<0, new11(D,A,B,C).
new4(A,B,C) :- A-C=< -1, new5(A,B,C).
new4(A,B,C) :- A-C>=0, new6(A,B,C).
new3(A,B,C) :- D=1, B-C=< -1, new4(D,B,C).
new2 :- A=1, new3(B,A,C).
new1 :- new2.
false :- new1.
