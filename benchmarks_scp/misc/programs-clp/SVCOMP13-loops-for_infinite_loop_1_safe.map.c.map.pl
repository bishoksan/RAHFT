new11(A,B,C,D,E,F) :- A=0.
new11(A,B,C,D,E,F) :- G=1+B, A=< -1, new8(G,C,D,E,F).
new11(A,B,C,D,E,F) :- G=1+B, A>=1, new8(G,C,D,E,F).
new10(A,B,C,D,E) :- F=1, B=0, new11(F,A,B,C,D,E).
new10(A,B,C,D,E) :- F=0, B=< -1, new11(F,A,B,C,D,E).
new10(A,B,C,D,E) :- F=0, B>=1, new11(F,A,B,C,D,E).
new8(A,B,C,D,E) :- new10(A,B,C,D,E).
new7(A,B,C,D,E,F) :- new7(A,B,C,D,E,F).
new5(A,B,C,D,E,F) :- A=0, new7(A,B,C,D,E,F).
new5(A,B,C,D,E,F) :- G=0, A=< -1, new8(G,C,D,E,F).
new5(A,B,C,D,E,F) :- G=0, A>=1, new8(G,C,D,E,F).
new4(A,B,C,D,E) :- F=1, D>=1, new5(F,A,B,C,D,E).
new4(A,B,C,D,E) :- F=0, D=<0, new5(F,A,B,C,D,E).
new3(A,B,C,D,E) :- new4(A,B,C,F,F).
new2 :- A=0, B=0, C=0, new3(B,C,A,D,E).
new1 :- new2.
false :- new1.
