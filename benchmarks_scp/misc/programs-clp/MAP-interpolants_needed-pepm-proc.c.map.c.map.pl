new13(A,B,C,D,E) :- F=1+A, G=3+B, A>=4, new3(F,G,C,D,E).
new13(A,B,C,D,E) :- A=<3, new3(A,B,C,D,E).
new12(A,B,C,D,E) :- C=< -1, new13(A,B,C,D,E).
new12(A,B,C,D,E) :- C>=1, new13(A,B,C,D,E).
new12(A,B,C,D,E) :- C=0, new3(A,B,C,D,E).
new11(A,B,C,D,E) :- new12(A,B,F,D,E).
new8(A,B,C,D,E) :- F=1+A, G=2+B, D=< -1, new3(F,G,C,D,E).
new8(A,B,C,D,E) :- F=1+A, G=2+B, D>=1, new3(F,G,C,D,E).
new8(A,B,C,D,E) :- D=0, new11(A,B,C,D,E).
new7(A,B,C,D,E) :- B-3*A>=1.
new5(A,B,C,D,E) :- new8(A,B,C,F,E).
new4(A,B,C,D,E) :- E=< -1, new5(A,B,C,D,E).
new4(A,B,C,D,E) :- E>=1, new5(A,B,C,D,E).
new4(A,B,C,D,E) :- E=0, new7(A,B,C,D,E).
new3(A,B,C,D,E) :- new4(A,B,C,D,F).
new2 :- A=0, B=0, new3(A,B,C,D,E).
new1 :- new2.
false :- new1.
