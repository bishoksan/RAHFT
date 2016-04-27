new4(A,B,C) :- A=0.
new3(A,B) :- C=1, A>=0, new4(C,A,B).
new3(A,B) :- C=0, A=< -1, new4(C,A,B).
new2 :- A=0, B=99, new3(A,B).
new1 :- new2.
false :- new1.
