new14(A,B,C,D,E) :- A=0.
new9(A,B,C,D) :- E=0, B=10, new8(A,B,C,E).
new9(A,B,C,D) :- E=1, B=<9, new8(A,B,C,E).
new9(A,B,C,D) :- E=1, B>=11, new8(A,B,C,E).
new8(A,B,C,D) :- new14(D,A,B,C,D).
new5(A,B,C,D) :- E=0, A=5, new8(A,B,C,E).
new5(A,B,C,D) :- A=<4, new9(A,B,C,D).
new5(A,B,C,D) :- A>=6, new9(A,B,C,D).
new4(A,B,C,D) :- E=5, C=< -1, new5(E,B,C,D).
new4(A,B,C,D) :- E=5, C>=1, new5(E,B,C,D).
new4(A,B,C,D) :- E=10, C=0, new5(A,E,C,D).
new3(A,B,C,D) :- new4(A,B,E,D).
new2 :- A=0, B=0, new3(B,A,C,D).
new1 :- new2.
false :- new1.
