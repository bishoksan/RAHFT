new9(A,B,C) :- new3(A,B,C).
new6(A,B,C,D) :- A=0.
new5(A,B,C) :- D=1, C>=1, new6(D,A,B,C).
new5(A,B,C) :- D=0, C=<0, new6(D,A,B,C).
new4(A,B,C) :- D= -1+B, E=2*C, B>=1, new4(A,D,E).
new4(A,B,C) :- B=<0, new9(A,B,C).
new3(A,B,C) :- A-C>=1, new4(A,B,C).
new3(A,B,C) :- A-C=<0, new5(A,B,C).
new2 :- A=10, B=1, new3(C,A,B).
new1 :- new2.
false :- new1.
