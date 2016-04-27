new9(A,B,C,D,E) :- A=0.
new8(A,B,C,D) :- E=1, A=<1, new9(E,A,B,C,D).
new8(A,B,C,D) :- E=1, A>=3, new9(E,A,B,C,D).
new8(A,B,C,D) :- E=0, A=2, new9(E,A,B,C,D).
new6(A,B,C,D) :- new8(A,B,C,D).
new4(A,B,C,D) :- E=1, C>=1, new6(E,B,C,D).
new4(A,B,C,D) :- E=2, C=<0, new6(E,B,C,D).
new3(A,B,C,D) :- E=2, D>=1, new4(A,E,C,D).
new3(A,B,C,D) :- E=3, D=<0, new4(A,E,C,D).
new2 :- new3(A,B,C,D).
new1 :- new2.
false :- new1.
