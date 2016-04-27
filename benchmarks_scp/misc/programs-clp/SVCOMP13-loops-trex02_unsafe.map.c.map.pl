new13(A,B,C) :- D= -1+A, new4(D,B,C).
new11(A,B,C) :- D= -1+A, new4(D,B,C).
new10(A,B,C) :- B=< -1, new11(A,B,C).
new10(A,B,C) :- B>=1, new11(A,B,C).
new10(A,B,C) :- B=0, new13(A,B,C).
new7(A,B,C,D) :- B=0.
new6(A,B,C) :- D=1, A=0, new7(A,D,B,C).
new6(A,B,C) :- D=0, A=< -1, new7(A,D,B,C).
new6(A,B,C) :- D=0, A>=1, new7(A,D,B,C).
new5(A,B,C) :- new10(A,D,D).
new4(A,B,C) :- A>=1, new5(A,B,C).
new4(A,B,C) :- A=<0, new6(A,B,C).
new3(A,B,C) :- new4(D,B,C).
new2(A) :- new3(A,B,C).
new1 :- new2(A).
false :- new1.
