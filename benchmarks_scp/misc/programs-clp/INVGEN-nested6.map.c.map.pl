new21(A,B,C,D,E,F) :- B=0.
new21(A,B,C,D,E,F) :- G=1+E, B=< -1, new8(A,C,D,G,F).
new21(A,B,C,D,E,F) :- G=1+E, B>=1, new8(A,C,D,G,F).
new19(A,B,C,D,E) :- F=1, 2*B-1*D=<0, new21(A,F,B,C,D,E).
new19(A,B,C,D,E) :- F=0, 2*B-1*D>=1, new21(A,F,B,C,D,E).
new15(A,B,C,D,E,F) :- B=0.
new15(A,B,C,D,E,F) :- G=1+D, B=< -1, new5(A,C,G,E,F).
new15(A,B,C,D,E,F) :- G=1+D, B>=1, new5(A,C,G,E,F).
new13(A,B,C,D,E) :- F=1, D-E=<0, new15(A,F,B,C,D,E).
new13(A,B,C,D,E) :- F=0, D-E>=1, new15(A,F,B,C,D,E).
new11(A,B,C,D,E,F) :- B=0.
new11(A,B,C,D,E,F) :- B=< -1, new13(A,C,D,E,F).
new11(A,B,C,D,E,F) :- B>=1, new13(A,C,D,E,F).
new10(A,B,C,D,E) :- F=1, D-E>=0, new11(A,F,B,C,D,E).
new10(A,B,C,D,E) :- F=0, D-E=< -1, new11(A,F,B,C,D,E).
new8(A,B,C,D,E) :- D-E=< -1, new19(A,B,C,D,E).
new8(A,B,C,D,E) :- F=1+C, D-E>=0, new5(A,B,F,D,E).
new6(A,B,C,D,E) :- A=< -1, new8(A,B,C,C,E).
new6(A,B,C,D,E) :- A>=1, new8(A,B,C,C,E).
new6(A,B,C,D,E) :- A=0, new10(A,B,C,D,E).
new5(A,B,C,D,E) :- C-E=< -1, new6(A,B,C,D,E).
new5(A,B,C,D,E) :- F=1+B, C-E>=0, new4(A,F,C,D,E).
new4(A,B,C,D,E) :- F=2*B, B-E=< -1, new5(A,B,F,D,E).
new3(A,B,C,D,D) :- E=0, new4(A,E,C,D,D).
new2(A) :- new3(A,B,C,D,E).
new1 :- new2(A).
false :- new1.
