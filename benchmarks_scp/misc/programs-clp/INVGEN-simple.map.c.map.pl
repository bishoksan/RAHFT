new11(A,B,C) :- A=0.
new10(A,B) :- C=1, A-B=<0, new11(C,A,B).
new10(A,B) :- C=0, A-B>=1, new11(C,A,B).
new7(A,B) :- C=1+A, A-B=< -1, new7(C,B).
new7(A,B) :- A-B>=0, new10(A,B).
new6(A,B,C) :- new6(A,B,C).
new4(A,B,C) :- A=0, new6(A,B,C).
new4(A,B,C) :- A=< -1, new7(B,C).
new4(A,B,C) :- A>=1, new7(B,C).
new3(A,B) :- C=1, B>=1, new4(C,A,B).
new3(A,B) :- C=0, B=<0, new4(C,A,B).
new2 :- A=0, new3(A,B).
new1 :- new2.
false :- new1.
