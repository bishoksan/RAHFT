new6(A,B,C,D) :- A=0.
new5(A,B,C) :- D=1, B>=1, new6(D,A,B,C).
new5(A,B,C) :- D=0, B=<0, new6(D,A,B,C).
new4(A,B,C) :- D=1+A, A-C=< -1, new4(D,B,C).
new4(A,B,C) :- D=1+B, A-C>=0, new3(A,D,C).
new3(A,B,C) :- D=1, B-C=< -1, new4(D,B,C).
new3(A,B,C) :- B-C>=0, new5(A,B,C).
new2 :- A=1, new3(B,A,C).
new1 :- new2.
false :- new1.
