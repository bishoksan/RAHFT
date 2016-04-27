new17(A,B,C,D) :- A=0.
new14(A,B,C,D,E,F,G) :- H=5+B+C, I=B+C, A-B>=0, new9(H,F,I).
new14(A,B,C,D,E,F,G) :- H=5+C, A-B=< -1, new9(H,F,C).
new12(A,B,C) :- D=1, E=5, F=1, G=0, new14(E,F,G,D,A,B,C).
new11(A,B,C,D,E,F,G) :- H= -1+B+C, I=B+C, A-B>=0, new12(H,I,G).
new11(A,B,C,D,E,F,G) :- H= -1+C, A-B=< -1, new12(H,C,G).
new10(A,B,C) :- D=1, E=1, F=0, new11(A,E,F,D,A,B,C).
new9(A,B,C) :- D=1, A=<0, new17(D,A,B,C).
new9(A,B,C) :- D=0, A>=1, new17(D,A,B,C).
new7(A,B,C) :- D=1+A, A>=1, new9(D,B,C).
new7(A,B,C) :- A=<0, new10(A,B,C).
new6(A,B,C,D) :- new6(A,B,C,D).
new4(A,B,C,D) :- A=0, new6(A,B,C,D).
new4(A,B,C,D) :- A=< -1, new7(B,C,D).
new4(A,B,C,D) :- A>=1, new7(B,C,D).
new3(A,B,C) :- D=1, A>=1, new4(D,A,B,C).
new3(A,B,C) :- D=0, A=<0, new4(D,A,B,C).
new2 :- new3(A,B,C).
new1 :- new2.
false :- new1.
