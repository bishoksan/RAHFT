new8(A,B,C,D,E) :- A=0.
new7(A,B,C,D) :- E=1, D=<2, new8(E,A,B,C,D).
new7(A,B,C,D) :- E=0, D>=3, new8(E,A,B,C,D).
new4(A,B,C,D) :- E=1+B, B-C=< -1, new4(A,E,C,D).
new4(A,B,C,D) :- B-C>=0, new7(A,B,C,D).
new3(A,B,C,D) :- E=1, A>=1, new4(A,B,C,E).
new3(A,B,C,D) :- E=2, A=<0, new4(A,B,C,E).
new2 :- new3(A,B,C,D).
new1 :- new2.
false :- new1.
