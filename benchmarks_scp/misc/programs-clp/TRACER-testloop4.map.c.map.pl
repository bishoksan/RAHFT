new7(A,B,C,D) :- A=0.
new6(A,B,C) :- D=1, C=<1, new7(D,A,B,C).
new6(A,B,C) :- D=0, C>=2, new7(D,A,B,C).
new3(A,B,C) :- D=1+A, A-B=< -1, new3(D,B,C).
new3(A,B,C) :- D=1+A, A-B>=1, new3(D,B,C).
new3(A,A,B) :- new6(A,A,B).
new2 :- A=1, B=10, C=0, new3(A,B,C).
new1 :- new2.
false :- new1.
