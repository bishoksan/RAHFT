new10(A,B,C) :- B-A>=1.
new9(A,B,C) :- D= -1+B, A-C>=1, new7(A,D,C).
new9(A,B,C) :- D=2+B, A-C=<0, new7(A,D,C).
new7(A,B,C) :- D=1+A, A-2*C=< -1, new9(D,B,C).
new7(A,B,C) :- A-2*C>=0, new10(A,B,C).
new6(A,B,C,D) :- new6(A,B,C,D).
new4(A,B,C,D) :- A=0, new6(A,B,C,D).
new4(A,B,C,D) :- A=< -1, new7(B,C,D).
new4(A,B,C,D) :- A>=1, new7(B,C,D).
new3(A,B,C) :- D=1, C>=1, new4(D,A,B,C).
new3(A,B,C) :- D=0, C=<0, new4(D,A,B,C).
new2 :- A=0, B=0, new3(B,A,C).
new1 :- new2.
false :- new1.
