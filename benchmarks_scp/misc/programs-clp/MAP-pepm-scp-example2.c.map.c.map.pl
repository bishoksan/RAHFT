new6(A,B,C,D) :- D=0.
new5(A,B,C) :- D=1, A-B=<0, new6(A,B,C,D).
new5(A,B,C) :- D=0, A-B>=1, new6(A,B,C,D).
new4(A,B,C) :- D=A+B, new3(A,D,C).
new3(A,B,C) :- D=1+A, A-C=< -1, new4(D,B,C).
new3(A,B,C) :- A-C>=0, new5(A,B,C).
new2(A,B,C) :- new3(A,B,C).
new1 :- A=0, B=0, new2(B,A,C).
false :- new1.
