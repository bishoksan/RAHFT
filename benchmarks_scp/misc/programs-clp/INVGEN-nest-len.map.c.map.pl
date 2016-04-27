new20(A,B,C) :- D=1+A, A-C=< -1, new20(D,B,C).
new20(A,B,C) :- D=1+B, A-C>=0, new3(A,D,C).
new18(A,B,C) :- D=1+A, A-C=< -1, new18(D,B,C).
new18(A,B,C) :- D=1, A-C>=0, new20(D,B,C).
new16(A,B,C) :- D=1+A, A-C=< -1, new16(D,B,C).
new16(A,B,C) :- D=1, A-C>=0, new18(D,B,C).
new14(A,B,C) :- D=1+A, A-C=< -1, new14(D,B,C).
new14(A,B,C) :- D=1, A-C>=0, new16(D,B,C).
new12(A,B,C) :- D=1+A, A-C=< -1, new12(D,B,C).
new12(A,B,C) :- D=1, A-C>=0, new14(D,B,C).
new10(A,B,C) :- D=1+A, A-C=< -1, new10(D,B,C).
new10(A,B,C) :- D=1, A-C>=0, new12(D,B,C).
new7(A,B,C) :- D=1+A, A-C=< -1, new7(D,B,C).
new7(A,B,C) :- D=1, A-C>=0, new10(D,B,C).
new5(A,B,C,D) :- A=0.
new5(A,B,C,D) :- E=1, A=< -1, new7(E,C,D).
new5(A,B,C,D) :- E=1, A>=1, new7(E,C,D).
new4(A,B,C) :- D=1, B>=1, new5(D,A,B,C).
new4(A,B,C) :- D=0, B=<0, new5(D,A,B,C).
new3(A,B,C) :- B-C=< -1, new4(A,B,C).
new2 :- A=1, new3(B,A,C).
new1 :- new2.
false :- new1.
