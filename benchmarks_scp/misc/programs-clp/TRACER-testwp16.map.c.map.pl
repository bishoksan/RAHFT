new7(A,B,C,D,E) :- A=0.
new5(A,B,C,D) :- E=1, A=<1000, new7(E,A,B,C,D).
new5(A,B,C,D) :- E=0, A>=1001, new7(E,A,B,C,D).
new4(A,B,C,D) :- E=2+A, C=3, new5(E,B,C,D).
new4(A,B,C,D) :- E=1+A, C=<2, new5(E,B,C,D).
new4(A,B,C,D) :- E=1+A, C>=4, new5(E,B,C,D).
new3(A,B,C,D) :- B=0, new4(A,B,C,D).
new3(A,B,C,D) :- E=1+A, B=< -1, new5(E,B,C,D).
new3(A,B,C,D) :- E=1+A, B>=1, new5(E,B,C,D).
new2 :- A=1, new3(A,B,B,B).
new1 :- new2.
false :- new1.
