new9(A,B,C,D,E,F) :- B=0.
new9(A,B,C,D,E,F) :- G=1+E, B=< -1, new5(A,C,D,G,F).
new9(A,B,C,D,E,F) :- G=1+E, B>=1, new5(A,C,D,G,F).
new7(A,B,C,D,E) :- F=1, B-D=<0, new9(A,F,B,C,D,E).
new7(A,B,C,D,E) :- F=0, B-D>=1, new9(A,F,B,C,D,E).
new5(A,B,C,D,E) :- D-E=< -1, new7(A,B,C,D,E).
new5(A,B,C,D,E) :- F=1+C, D-E>=0, new4(A,B,F,D,E).
new4(A,B,C,D,E) :- C-E=< -1, new5(A,B,C,C,E).
new4(A,B,C,D,E) :- F=1+B, C-E>=0, new3(A,F,C,D,E).
new3(A,B,C,D,E) :- B-E=< -1, new4(A,B,B,D,E).
new2(A) :- B=0, new3(A,B,C,D,E).
new1 :- new2(A).
false :- new1.
