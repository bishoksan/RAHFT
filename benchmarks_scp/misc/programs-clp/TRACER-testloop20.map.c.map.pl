new16(A,B,C,D) :- A=0.
new16(A,B,C,D) :- E=5, A=< -1, new13(B,E,D).
new16(A,B,C,D) :- E=5, A>=1, new13(B,E,D).
new13(A,B,C) :- new3(A,B,C).
new12(A,B,C) :- D=1, A=<2, new16(D,A,B,C).
new12(A,B,C) :- D=1, A>=4, new16(D,A,B,C).
new12(A,B,C) :- D=0, A=3, new16(D,A,B,C).
new10(A,B,C) :- B=4, new12(A,B,C).
new10(A,B,C) :- B=<3, new13(A,B,C).
new10(A,B,C) :- B>=5, new13(A,B,C).
new9(A,B,C) :- D=4, E=2, A=1, new13(E,D,C).
new9(A,B,C) :- D=4, A=<0, new13(A,D,C).
new9(A,B,C) :- D=4, A>=2, new13(A,D,C).
new7(A,B,C) :- B=3, new9(A,B,C).
new7(A,B,C) :- B=<2, new10(A,B,C).
new7(A,B,C) :- B>=4, new10(A,B,C).
new6(A,B,C) :- D=3, E=1, A=0, new13(E,D,C).
new6(A,B,C) :- D=3, A=< -1, new13(A,D,C).
new6(A,B,C) :- D=3, A>=1, new13(A,D,C).
new4(A,B,C) :- B=2, new6(A,B,C).
new4(A,B,C) :- B=<1, new7(A,B,C).
new4(A,B,C) :- B>=3, new7(A,B,C).
new3(A,B,C) :- C=< -1, new4(A,B,C).
new3(A,B,C) :- C>=1, new4(A,B,C).
new2 :- A=2, B=0, new3(B,A,C).
new1 :- new2.
false :- new1.
