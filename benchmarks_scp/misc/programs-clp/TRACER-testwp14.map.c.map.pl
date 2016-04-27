new11(A,B,C,D) :- A=0.
new9(A,B,C) :- D=1, B>=5, new11(D,A,B,C).
new9(A,B,C) :- D=0, B=<4, new11(D,A,B,C).
new8(A,B,C,D) :- new8(A,B,C,D).
new6(A,B,C,D) :- A=0, new8(A,B,C,D).
new6(A,B,C,D) :- A=< -1, new9(B,B,D).
new6(A,B,C,D) :- A>=1, new9(B,B,D).
new4(A,B,C) :- D=1, A>=1, new6(D,A,B,C).
new4(A,B,C) :- D=0, A=<0, new6(D,A,B,C).
new3(A,B,C) :- D=4, A>=5, new4(A,B,D).
new3(A,B,C) :- D=5, A=<4, new4(D,B,C).
new2 :- new3(A,B,C).
new1 :- new2.
false :- new1.
