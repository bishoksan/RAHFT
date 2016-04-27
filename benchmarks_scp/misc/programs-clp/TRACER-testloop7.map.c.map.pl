new9(A,B,C) :- D=0, B=<0, new3(A,D,C).
new9(A,B,C) :- B>=1, new3(A,B,C).
new7(A,B,C,D) :- A=0.
new6(A,B,C) :- D=1, A=<2, new7(D,A,B,C).
new6(A,B,C) :- D=0, A>=3, new7(D,A,B,C).
new5(A,B,C) :- D=2, A=<1, new9(D,B,C).
new5(A,B,C) :- D=1, A>=2, new9(D,B,C).
new4(A,B,C) :- C=<9, new5(A,B,C).
new4(A,B,C) :- C>=10, new6(A,B,C).
new3(A,B,C) :- new4(A,B,D).
new2 :- A=1, B=0, new3(A,B,C).
new1 :- new2.
false :- new1.
