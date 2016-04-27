new15(A,B,C,D,E) :- A=0.
new14(A,B,C,D) :- new15(D,A,B,C,D).
new13(A,B,C,D) :- E=1, B=<0, new14(A,B,C,E).
new13(A,B,C,D) :- E=0, B>=1, new14(A,B,C,E).
new12(A,B,C,D) :- B>=0, new13(A,B,C,D).
new12(A,B,C,D) :- E=0, B=< -1, new14(A,B,C,E).
new10(A,B,C,D) :- E= -1+B, F= -1+A, A=< -1, new7(F,E,C,D).
new10(A,B,C,D) :- A>=0, new12(A,B,C,D).
new8(A,B,C,D) :- E= -1+B, F= -1+A, A>=1, new7(F,E,C,D).
new8(A,B,C,D) :- A=<0, new10(A,B,C,D).
new7(A,B,C,D) :- new8(A,B,C,D).
new4(A,B,C,D) :- E=1+A, F=1+B, C=< -1, new3(E,F,C,D).
new4(A,B,C,D) :- E=1+A, F=1+B, C>=1, new3(E,F,C,D).
new4(A,B,C,D) :- C=0, new7(A,B,C,D).
new3(A,B,C,D) :- new4(A,B,E,D).
new2 :- A=0, B=0, new3(A,B,C,D).
new1 :- new2.
false :- new1.
