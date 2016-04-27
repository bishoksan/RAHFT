new8(A,B,C) :- A=0.
new6(A,B) :- C=1, A=<50, new8(C,A,B).
new6(A,B) :- C=0, A>=51, new8(C,A,B).
new4(A,B) :- C=3+A, A>=1, new6(C,B).
new4(A,B) :- C=5+A, A=<0, new6(C,B).
new3(A,B) :- C=1, B>=1, new4(C,B).
new3(A,B) :- C=47, B=<0, new4(C,B).
new2 :- new3(A,B).
new1 :- new2.
false :- new1.
