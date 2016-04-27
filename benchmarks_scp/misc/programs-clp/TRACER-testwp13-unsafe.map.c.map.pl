new8(A,B,C) :- A=0.
new7(A,B) :- C=1, A>=0, new8(C,A,B).
new7(A,B) :- C=0, A=< -1, new8(C,A,B).
new5(A,B) :- A=<49, new7(A,B).
new4(A,B) :- C=1+B, B>=1, new5(A,C).
new4(A,B) :- C= -10+A, B=<0, new5(C,B).
new3(A,B) :- A>=6, new4(A,B).
new2 :- new3(A,B).
new1 :- new2.
false :- new1.
