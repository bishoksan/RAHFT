new8(A,B,C,D) :- A=0.
new6(A,B,C) :- D=1, A+B=<50, new8(D,A,B,C).
new6(A,B,C) :- D=0, A+B>=51, new8(D,A,B,C).
new4(A,B,C) :- D=1+A, C>=1, new6(D,B,C).
new4(A,B,C) :- D=4+A, C=<0, new6(D,B,C).
new3(A,B,C) :- D=2, B>=1, new4(A,D,C).
new3(A,B,C) :- D=5, B=<0, new4(A,D,C).
new2 :- A=0, new3(A,B,C).
new1 :- new2.
false :- new1.
