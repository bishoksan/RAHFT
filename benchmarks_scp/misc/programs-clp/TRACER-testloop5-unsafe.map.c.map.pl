new7(A,B,C) :- B=0.
new6(A,B) :- C=1, A=<9, new7(A,C,B).
new6(A,B) :- C=1, A>=11, new7(A,C,B).
new6(A,B) :- C=0, A=10, new7(A,C,B).
new4(A,B) :- B=<9, new3(A,B).
new4(A,B) :- B>=10, new6(A,B).
new3(A,B) :- C=1+A, D=1+A, new4(D,C).
new2(A) :- new3(A,B).
new1 :- A=0, new2(A).
false :- new1.
