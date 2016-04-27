new12(A,B,C,D,E) :- A=0.
new9(A,B,C,D) :- E=0, C>=11, new8(A,B,C,E).
new9(A,B,C,D) :- E=1, C=<10, new8(A,B,C,E).
new8(A,B,C,D) :- new12(D,A,B,C,D).
new7(A,B,C,D) :- E=0, B>=11, new8(A,B,C,E).
new7(A,B,C,D) :- B=<10, new9(A,B,C,D).
new5(A,B,C,D) :- E=1+B, A-B>=1, new5(A,E,C,D).
new5(A,B,C,D) :- A-B=<0, new7(A,B,C,D).
new3(A,B,C,D) :- E=1+C, A-C>=1, new3(A,B,E,D).
new3(A,B,C,D) :- A-C=<0, new5(A,B,C,D).
new2 :- A=10, B=0, C=0, new3(A,B,C,D).
new1 :- new2.
false :- new1.
