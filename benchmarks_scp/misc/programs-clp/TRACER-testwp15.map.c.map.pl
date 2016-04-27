new9(A,B,C,D) :- B=0.
new7(A,B,C) :- D=1, B+C>=1, new9(A,D,B,C).
new7(A,B,C) :- D=0, B+C=<0, new9(A,D,B,C).
new6(A,B,C,D) :- E=3, B>=1, new7(E,C,B).
new6(A,B,C,D) :- E=1, B=<0, new7(A,C,E).
new4(A,B,C) :- new6(A,D,B,C).
new3(A,B,C) :- D=2, B>=1, new4(D,B,C).
new3(A,B,C) :- D=0, B=<0, new4(A,D,C).
new2(A) :- new3(A,B,C).
new1 :- new2(A).
false :- new1.
