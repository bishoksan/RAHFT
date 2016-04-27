new8(A,B,C,D,E,F) :- A=0.
new7(A,B,C,D,E) :- F=1, E=<0, new8(F,A,B,C,D,E).
new7(A,B,C,D,E) :- F=0, E>=1, new8(F,A,B,C,D,E).
new6(A,B,C,D,E) :- new4(A,B,C,D,E).
new4(A,B,C,D,E) :- F=1+B, B-C=< -1, new6(A,F,C,D,E).
new4(A,B,C,D,E) :- B-C>=0, new7(A,B,C,D,E).
new3(A,B,C,D,E) :- F=1, A>=1, new4(A,B,C,F,E).
new3(A,B,C,D,E) :- F=2, A=<0, new4(A,B,C,F,E).
new2 :- A=0, B=0, new3(C,A,D,E,B).
new1 :- new2.
false :- new1.
