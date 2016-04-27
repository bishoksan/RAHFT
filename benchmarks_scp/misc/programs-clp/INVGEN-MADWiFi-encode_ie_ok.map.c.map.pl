new15(A,B,C,D,E,F,G) :- A=0.
new15(A,B,C,D,E,F,G) :- H=2+B, I=1+C, A=< -1, new8(H,I,D,E,F,G).
new15(A,B,C,D,E,F,G) :- H=2+B, I=1+C, A>=1, new8(H,I,D,E,F,G).
new13(A,B,C,D,E,F) :- G=1, A-E=< -2, new15(G,A,B,C,D,E,F).
new13(A,B,C,D,E,F) :- G=0, A-E>= -1, new15(G,A,B,C,D,E,F).
new11(A,B,C,D,E,F,G) :- A=0.
new11(A,B,C,D,E,F,G) :- A=< -1, new13(B,C,D,E,F,G).
new11(A,B,C,D,E,F,G) :- A>=1, new13(B,C,D,E,F,G).
new10(A,B,C,D,E,F) :- G=1, A>=0, new11(G,A,B,C,D,E,F).
new10(A,B,C,D,E,F) :- G=0, A=< -1, new11(G,A,B,C,D,E,F).
new9(A,B,C,D,E,F) :- D>=3, new10(A,B,C,D,E,F).
new8(A,B,C,D,E,F) :- B-F=< -1, new9(A,B,C,D,E,F).
new7(A,B,C,D,E,F) :- G=0, D-2*F>=0, new8(A,G,C,D,E,F).
new6(A,B,C,D,E,F) :- G= -(C)+D, C-D=<0, new7(C,B,C,G,D,F).
new5(A,B,C,D,E,F) :- F>=1, new6(A,B,C,D,E,F).
new4(A,B,C,D,E,F) :- D>=1, new5(A,B,C,D,E,F).
new3(A,B,C,D,E,F) :- C>=1, new4(A,B,C,D,E,F).
new2 :- new3(A,B,C,D,E,F).
new1 :- new2.
false :- new1.
