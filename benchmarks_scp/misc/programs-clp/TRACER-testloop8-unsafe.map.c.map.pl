new10(A,B,C,D) :- A=0.
new9(A,B,C) :- D=1, B=< -1, new10(D,A,B,C).
new9(A,B,C) :- D=1, B>=1, new10(D,A,B,C).
new9(A,B,C) :- D=0, B=0, new10(D,A,B,C).
new7(A,B,C) :- D= -1+A, E= -1+B, A>=1, new7(D,E,C).
new7(A,B,C) :- A=<0, new9(A,B,C).
new4(A,B,C) :- D=1+A, E=1+B, C=< -1, new3(D,E,C).
new4(A,B,C) :- D=1+A, E=1+B, C>=1, new3(D,E,C).
new4(A,B,C) :- C=0, new7(A,B,C).
new3(A,B,C) :- new4(A,B,D).
new2 :- A=0, B=0, new3(A,B,C).
new1 :- new2.
false :- new1.
