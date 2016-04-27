new6(A,B,C) :- A=0.
new5(A,B) :- C=1, A-B=<0, new6(C,A,B).
new5(A,B) :- C=0, A-B>=1, new6(C,A,B).
new3(A,B) :- C=1+A, A-B=< -1, new3(C,B).
new3(A,B) :- A-B>=0, new5(A,B).
new2 :- A=0, B=100, new3(A,B).
new1 :- new2.
false :- new1.
