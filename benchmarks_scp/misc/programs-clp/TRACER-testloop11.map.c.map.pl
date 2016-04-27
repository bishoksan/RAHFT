new16(A,B,C,D) :- A=0.
new16(A,B,C,D) :- E=5, A=< -1, new3(B,E,D).
new16(A,B,C,D) :- E=5, A>=1, new3(B,E,D).
new13(A,B,C) :- D=1, A=<2, new16(D,A,B,C).
new13(A,B,C) :- D=1, A>=4, new16(D,A,B,C).
new13(A,B,C) :- D=0, A=3, new16(D,A,B,C).
new11(A,B,C) :- B=4, new13(A,B,C).
new11(A,B,C) :- B=<3, new3(A,B,C).
new11(A,B,C) :- B>=5, new3(A,B,C).
new10(A,B,C) :- D=2, E=4, A=1, new3(D,E,C).
new10(A,B,C) :- D=4, A=<0, new3(A,D,C).
new10(A,B,C) :- D=4, A>=2, new3(A,D,C).
new8(A,B,C) :- B=3, new10(A,B,C).
new8(A,B,C) :- B=<2, new11(A,B,C).
new8(A,B,C) :- B>=4, new11(A,B,C).
new7(A,B,C) :- D=1, E=3, A=0, new3(D,E,C).
new7(A,B,C) :- D=3, A=< -1, new3(A,D,C).
new7(A,B,C) :- D=3, A>=1, new3(A,D,C).
new5(A,B,C) :- B=2, new7(A,B,C).
new5(A,B,C) :- B=<1, new8(A,B,C).
new5(A,B,C) :- B>=3, new8(A,B,C).
new4(A,B,C) :- C=< -1, new5(A,B,C).
new4(A,B,C) :- C>=1, new5(A,B,C).
new3(A,B,C) :- new4(A,B,D).
new2 :- A=0, B=2, new3(A,B,C).
new1 :- new2.
false :- new1.
