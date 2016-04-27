new6(A,B,C) :- A=0.
new5(A,B) :- C=1, B=<10, new6(C,A,B).
new5(A,B) :- C=0, B>=11, new6(C,A,B).
new3(A,B) :- C=1+B, A-B>=1, new3(A,C).
new3(A,B) :- A-B=<0, new5(A,B).
new2 :- A=10, B=0, new3(A,B).
new1 :- new2.
false :- new1.
