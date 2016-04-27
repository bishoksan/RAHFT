new14(A,B,C,D,E) :- A=0.
new14(A,B,C,D,E) :- F=1+B, A=< -1, new11(F,C,D,E).
new14(A,B,C,D,E) :- F=1+B, A>=1, new11(F,C,D,E).
new12(A,B,C,D) :- E=1, B>=1, new14(E,A,B,C,D).
new12(A,B,C,D) :- E=0, B=<0, new14(E,A,B,C,D).
new11(A,B,C,D) :- A-C=< -1, new12(A,B,C,D).
new11(A,B,C,D) :- E=1+B, A-C>=0, new7(A,E,C,D).
new9(A,B,C,D) :- E=1+A, A-C=< -1, new9(E,B,C,D).
new9(A,B,C,D) :- A-C>=0, new11(D,B,C,D).
new7(A,B,C,D) :- B-C=< -1, new9(D,B,C,D).
new6(A,B,C,D,E) :- new6(A,B,C,D,E).
new4(A,B,C,D,E) :- A=0, new6(A,B,C,D,E).
new4(A,B,C,D,E) :- F=1, A=< -1, new7(B,F,D,E).
new4(A,B,C,D,E) :- F=1, A>=1, new7(B,F,D,E).
new3(A,B,C,D) :- E=1, D>=1, new4(E,A,B,C,D).
new3(A,B,C,D) :- E=0, D=<0, new4(E,A,B,C,D).
new2 :- new3(A,B,C,D).
new1 :- new2.
false :- new1.
