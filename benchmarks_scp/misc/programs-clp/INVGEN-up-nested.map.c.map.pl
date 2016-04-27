new15(A,B,C,D,E,F) :- new15(A,B,C,D,E,F).
new13(A,B,C,D,E,F) :- B=0, new15(A,B,C,D,E,F).
new13(A,B,C,D,E,F) :- G=1+D, B=< -1, new7(A,C,G,E,F).
new13(A,B,C,D,E,F) :- G=1+D, B>=1, new7(A,C,G,E,F).
new11(A,B,C,D,E,F) :- B=0.
new10(A,B,C,D,E) :- F=1, D>=0, new11(A,F,B,C,D,E).
new10(A,B,C,D,E) :- F=0, D=< -1, new11(A,F,B,C,D,E).
new9(A,B,C,D,E) :- F=1, D>=0, new13(A,F,B,C,D,E).
new9(A,B,C,D,E) :- F=0, D=< -1, new13(A,F,B,C,D,E).
new7(A,B,C,D,E) :- B-C>=0, new9(A,B,C,D,E).
new7(A,B,C,D,E) :- B-C=< -1, new10(A,B,C,D,E).
new6(A,B,C,D,E,F) :- new6(A,B,C,D,E,F).
new4(A,B,C,D,E,F) :- B=0, new6(A,B,C,D,E,F).
new4(A,B,C,D,E,F) :- B=< -1, new7(A,C,D,E,F).
new4(A,B,C,D,E,F) :- B>=1, new7(A,C,D,E,F).
new3(A,B,C,D,E) :- F=1, B-C>=0, new4(A,F,B,C,D,E).
new3(A,B,C,D,E) :- F=0, B-C=< -1, new4(A,F,B,C,D,E).
new2(A) :- B=0, C=0, new3(A,D,E,C,B).
new1 :- new2(A).
false :- new1.
