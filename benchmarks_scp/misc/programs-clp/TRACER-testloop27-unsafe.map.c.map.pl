new6(A,B,C,D) :- A=0.
new5(A,B,C) :- D=1, B=0, new6(D,A,B,C).
new5(A,B,C) :- D=0, B=< -1, new6(D,A,B,C).
new5(A,B,C) :- D=0, B>=1, new6(D,A,B,C).
new4(A,B,C) :- D=1+A, E= -1+B, C>=1, new3(D,E,C).
new4(A,B,C) :- D=1+A, C=<0, new3(D,B,C).
new3(A,B,C) :- D=1+B, A=<0, new4(A,D,C).
new3(A,B,C) :- A>=1, new5(A,B,C).
new2 :- A=0, B=0, new3(A,B,C).
new1 :- new2.
false :- new1.
