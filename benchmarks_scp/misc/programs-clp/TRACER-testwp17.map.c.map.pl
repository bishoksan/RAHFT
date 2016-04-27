new7(A,B,C) :- A=0.
new6(A,B) :- C=1, A=0, new7(C,A,B).
new6(A,B) :- C=0, A=< -1, new7(C,A,B).
new6(A,B) :- C=0, A>=1, new7(C,A,B).
new4(A,B) :- A=0, new6(A,B).
new3(A,B) :- C=0, B>=1, new4(C,B).
new3(A,B) :- C=1, B=<0, new4(C,B).
new2 :- new3(A,B).
new1 :- new2.
false :- new1.
