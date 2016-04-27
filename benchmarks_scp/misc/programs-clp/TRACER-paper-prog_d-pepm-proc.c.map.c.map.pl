new10(A,B) :- B+A=<9999.
new7(A,B) :- C=1+A, D=1+B, B=<9999, new7(C,D).
new7(A,B) :- B>=10000, new10(A,B).
new6(A,B,C) :- new6(A,B,C).
new4(A,B,C) :- A=0, new6(A,B,C).
new4(A,B,C) :- A=< -1, new7(B,C).
new4(A,B,C) :- A>=1, new7(B,C).
new3(A,B) :- C=1, A>=0, new4(C,A,B).
new3(A,B) :- C=0, A=< -1, new4(C,A,B).
new2 :- A=0, new3(B,A).
new1 :- new2.
false :- new1.
