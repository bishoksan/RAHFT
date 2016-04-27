new22(A,B,C,D,E,F,G) :- B=0.
new22(A,B,C,D,E,F,G) :- H=1+E, B=< -1, new17(A,C,D,H,F,G).
new22(A,B,C,D,E,F,G) :- H=1+E, B>=1, new17(A,C,D,H,F,G).
new20(A,B,C,D,E,F) :- new5(A,B,C,D,E,F).
new19(A,B,C,D,E,F) :- G=1, D-E=< -1, new22(A,G,B,C,D,E,F).
new19(A,B,C,D,E,F) :- G=0, D-E>=0, new22(A,G,B,C,D,E,F).
new17(A,B,C,D,E,F) :- C-D>=1, new19(A,B,C,D,E,F).
new17(A,B,C,D,E,F) :- C-D=<0, new20(A,B,C,D,E,F).
new15(A,B,C,D,E,F,G) :- B=0.
new15(A,B,C,D,E,F,G) :- H=1+D, I=0, B=< -1, new17(A,C,H,I,F,G).
new15(A,B,C,D,E,F,G) :- H=1+D, I=0, B>=1, new17(A,C,H,I,F,G).
new11(A,B,C,D,E,F,G) :- B=0.
new11(A,B,C,D,E,F,G) :- H=2+D, B=< -1, new5(A,C,H,E,F,G).
new11(A,B,C,D,E,F,G) :- H=2+D, B>=1, new5(A,C,H,E,F,G).
new10(A,B,C,D,E,F) :- G=1, B-C-E=<4, new11(A,G,B,C,D,E,F).
new10(A,B,C,D,E,F) :- G=0, B-C-E>=5, new11(A,G,B,C,D,E,F).
new8(A,B,C,D,E,F) :- G=1, C>=0, new15(A,G,B,C,D,E,F).
new8(A,B,C,D,E,F) :- G=0, C=< -1, new15(A,G,B,C,D,E,F).
new6(A,B,C,D,E,F) :- A=< -1, new8(A,B,C,D,E,F).
new6(A,B,C,D,E,F) :- A>=1, new8(A,B,C,D,E,F).
new6(A,B,C,D,E,F) :- A=0, new10(A,B,C,D,E,F).
new5(A,B,C,D,E,F) :- C-F=< -1, new6(A,B,C,D,E,F).
new5(A,B,C,D,E,F) :- G=4+B, C-F>=0, new4(A,G,C,D,E,F).
new4(A,B,C,D,E,F) :- B-E=< -1, new5(A,B,B,D,E,F).
new3(A,B,C,D,E,F) :- G=0, E-F>=2, new4(A,G,C,D,E,F).
new2(A) :- new3(A,B,C,D,E,F).
new1 :- new2(A).
false :- new1.
