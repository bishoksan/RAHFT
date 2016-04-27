new16(A,B,C,D,E) :- A=0.
new13(A,B,C,D) :- E=1, A=<7, new16(E,A,B,C,D).
new13(A,B,C,D) :- E=0, A>=8, new16(E,A,B,C,D).
new12(A,B,C,D) :- E=4+A, D=< -1, new13(E,B,C,D).
new12(A,B,C,D) :- E=4+A, D>=1, new13(E,B,C,D).
new12(A,B,C,D) :- D=0, new13(A,B,C,D).
new9(A,B,C,D) :- new12(A,B,C,E).
new8(A,B,C,D) :- E=2+A, C=< -1, new9(E,B,C,D).
new8(A,B,C,D) :- E=2+A, C>=1, new9(E,B,C,D).
new8(A,B,C,D) :- C=0, new9(A,B,C,D).
new5(A,B,C,D) :- new8(A,B,E,D).
new4(A,B,C,D) :- E=1+A, B=< -1, new5(E,B,C,D).
new4(A,B,C,D) :- E=1+A, B>=1, new5(E,B,C,D).
new4(A,B,C,D) :- B=0, new5(A,B,C,D).
new3(A,B,C,D) :- new4(A,E,C,D).
new2 :- A=0, new3(A,B,C,D).
new1 :- new2.
false :- new1.
