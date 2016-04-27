new15(A,B,C,D,E,F) :- A=0.
new15(A,B,C,D,E,F) :- G=1+B, A=< -1, new7(G,C,D,E,F).
new15(A,B,C,D,E,F) :- G=1+B, A>=1, new7(G,C,D,E,F).
new13(A,B,C,D,E) :- F=1, C>=0, new15(F,A,B,C,D,E).
new13(A,B,C,D,E) :- F=0, C=< -1, new15(F,A,B,C,D,E).
new11(A,B,C,D,E,F) :- A=0.
new10(A,B,C,D,E) :- F=1, E>=0, new11(F,A,B,C,D,E).
new10(A,B,C,D,E) :- F=0, E=< -1, new11(F,A,B,C,D,E).
new9(A,B,C,D,E) :- D>=1, new13(A,B,C,D,E).
new9(A,B,C,D,E) :- F=1+B, D=<0, new13(A,F,C,D,E).
new7(A,B,C,D,E) :- A=<9, new9(A,B,C,D,E).
new7(A,B,C,D,E) :- A>=10, new10(A,B,C,D,E).
new6(A,B,C,D,E,F) :- new6(A,B,C,D,E,F).
new4(A,B,C,D,E,F) :- A=0, new6(A,B,C,D,E,F).
new4(A,B,C,D,E,F) :- G=0, A=< -1, new7(G,C,D,E,F).
new4(A,B,C,D,E,F) :- G=0, A>=1, new7(G,C,D,E,F).
new3(A,B,C,D,E) :- F=1, C>=0, new4(F,A,B,C,D,E).
new3(A,B,C,D,E) :- F=0, C=< -1, new4(F,A,B,C,D,E).
new2 :- A=1, B=0, new3(C,B,D,E,A).
new1 :- new2.
false :- new1.
