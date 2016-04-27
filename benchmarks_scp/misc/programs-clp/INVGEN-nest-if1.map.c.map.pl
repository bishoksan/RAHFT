new17(A,B,C,D,E,F) :- B=0.
new17(A,B,C,D,E,F) :- G=1+C, B=< -1, new9(A,G,D,E,F).
new17(A,B,C,D,E,F) :- G=1+C, B>=1, new9(A,G,D,E,F).
new12(A,B,C,D,E) :- F=1+B, B-D=< -1, new12(A,F,C,D,E).
new12(A,B,C,D,E) :- F=1+C, B-D>=0, new7(A,B,F,D,E).
new11(A,B,C,D,E) :- A=< -1, new12(A,E,C,D,E).
new11(A,B,C,D,E) :- A>=1, new12(A,E,C,D,E).
new11(A,B,C,D,E) :- F=1+C, A=0, new7(A,B,F,D,E).
new10(A,B,C,D,E) :- F=1, B>=1, new17(A,F,B,C,D,E).
new10(A,B,C,D,E) :- F=0, B=<0, new17(A,F,B,C,D,E).
new9(A,B,C,D,E) :- B-D=< -1, new10(A,B,C,D,E).
new9(A,B,C,D,E) :- B-D>=0, new11(A,B,C,D,E).
new7(A,B,C,D,E) :- C-D=< -1, new9(A,E,C,D,E).
new6(A,B,C,D,E,F) :- new6(A,B,C,D,E,F).
new4(A,B,C,D,E,F) :- B=0, new6(A,B,C,D,E,F).
new4(A,B,C,D,E,F) :- G=1, B=< -1, new7(A,C,G,E,F).
new4(A,B,C,D,E,F) :- G=1, B>=1, new7(A,C,G,E,F).
new3(A,B,C,D,E) :- F=1, E>=1, new4(A,F,B,C,D,E).
new3(A,B,C,D,E) :- F=0, E=<0, new4(A,F,B,C,D,E).
new2(A) :- new3(A,B,C,D,E).
new1 :- new2(A).
false :- new1.
