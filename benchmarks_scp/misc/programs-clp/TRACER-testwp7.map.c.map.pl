new8(A,B,C,D) :- A=0.
new6(A,B,C) :- D=1, A+B>=1, new8(D,A,B,C).
new6(A,B,C) :- D=0, A+B=<0, new8(D,A,B,C).
new4(A,B,C) :- D=3, B>=1, new6(A,B,D).
new4(A,B,C) :- D=1, B=<0, new6(A,D,C).
new3(A,B,C) :- D=2, A>=1, new4(A,B,D).
new3(A,B,C) :- D=0, A=<0, new4(D,B,C).
new2 :- new3(A,B,C).
new1 :- new2.
false :- new1.
