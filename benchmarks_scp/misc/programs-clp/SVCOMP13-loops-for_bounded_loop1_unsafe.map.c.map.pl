new27(A,B,C,D,E,F) :- A=0.
new27(A,B,C,D,E,F) :- G=1+B, A=< -1, new8(G,C,D,E,F).
new27(A,B,C,D,E,F) :- G=1+B, A>=1, new8(G,C,D,E,F).
new25(A,B,C,D,E) :- F=1, B=< -1, new27(F,A,B,C,D,E).
new25(A,B,C,D,E) :- F=1, B>=1, new27(F,A,B,C,D,E).
new25(A,B,C,D,E) :- F=0, B=0, new27(F,A,B,C,D,E).
new24(A,B,C,D,E,F) :- new24(A,B,C,D,E,F).
new21(A,B,C,D,E,F) :- A=0, new24(A,B,C,D,E,F).
new21(A,B,C,D,E,F) :- G=C+D, A=< -1, new25(B,G,D,E,F).
new21(A,B,C,D,E,F) :- G=C+D, A>=1, new25(B,G,D,E,F).
new20(A,B,C,D,E) :- F=1, C=< -1, new21(F,A,B,C,D,E).
new20(A,B,C,D,E) :- F=1, C>=1, new21(F,A,B,C,D,E).
new20(A,B,C,D,E) :- F=0, C=0, new21(F,A,B,C,D,E).
new18(A,B,C,D,E) :- new20(A,B,F,D,E).
new15(A,B,C,D,E,F) :- A=0.
new15(A,B,C,D,E,F) :- A=< -1, new18(B,C,D,E,F).
new15(A,B,C,D,E,F) :- A>=1, new18(B,C,D,E,F).
new12(A,B,C,D,E,F) :- A=0.
new11(A,B,C,D,E) :- F=1, B=0, new12(F,A,B,C,D,E).
new11(A,B,C,D,E) :- F=0, B=< -1, new12(F,A,B,C,D,E).
new11(A,B,C,D,E) :- F=0, B>=1, new12(F,A,B,C,D,E).
new10(A,B,C,D,E) :- F=1, B=0, new15(F,A,B,C,D,E).
new10(A,B,C,D,E) :- F=0, B=< -1, new15(F,A,B,C,D,E).
new10(A,B,C,D,E) :- F=0, B>=1, new15(F,A,B,C,D,E).
new8(A,B,C,D,E) :- F=B-C, A-D=< -1, new10(A,F,C,D,E).
new8(A,B,C,D,E) :- A-D>=0, new11(A,B,C,D,E).
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
