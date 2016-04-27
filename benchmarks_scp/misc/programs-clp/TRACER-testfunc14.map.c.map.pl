new12(A,B,C) :- A=0.
new11(A,B) :- C=1, B=<1, new12(C,A,B).
new11(A,B) :- C=1, B>=3, new12(C,A,B).
new11(A,B) :- C=0, B=2, new12(C,A,B).
new7(A,B,C,D,E,F) :- G=1+A, B=< -1, new7(G,B,C,D,E,F).
new7(A,B,C,D,E,F) :- G=1+A, B>=1, new7(G,B,C,D,E,F).
new7(A,B,C,D,E,F) :- B=0, new11(E,F).
new6(A,B,C,D,E,F) :- G=4, C>=1, new7(A,B,C,G,E,F).
new6(A,B,C,D,E,F) :- G=5, C=<0, new7(A,B,C,G,E,F).
new4(A,B) :- C=0, new6(C,D,E,F,A,B).
new3(A,B) :- C=1, A>=1, new4(A,C).
new3(A,B) :- C=3, A=<0, new4(A,C).
new2 :- new3(A,B).
new1 :- new2.
false :- new1.
