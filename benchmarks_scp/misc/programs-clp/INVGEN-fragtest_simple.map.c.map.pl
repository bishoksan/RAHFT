new17(A,B,C,D,E,F,G) :- F-G>=1, new14(A,B,C,D,E,F,G).
new15(A,B,C,D,E,F,G,H) :- B=0.
new15(A,B,C,D,E,F,G,H) :- I=1+H, J= -1+C, K= -1+F, B=< -1, new17(A,J,D,E,K,G,I).
new15(A,B,C,D,E,F,G,H) :- I=1+H, J= -1+C, K= -1+F, B>=1, new17(A,J,D,E,K,G,I).
new14(A,B,C,D,E,F,G) :- H=1, E>=0, new15(A,H,B,C,D,E,F,G).
new14(A,B,C,D,E,F,G) :- H=0, E=< -1, new15(A,H,B,C,D,E,F,G).
new11(A,B,C,D,E,F,G) :- A=< -1, new11(A,B,C,D,E,F,G).
new11(A,B,C,D,E,F,G) :- A>=1, new11(A,B,C,D,E,F,G).
new11(A,B,C,D,E,F,G) :- H=0, A=0, new14(A,B,C,D,E,B,H).
new7(A,B,C,D,E,F,G) :- H=1+B, I=1+E, A=< -1, new7(A,H,C,B,I,F,G).
new7(A,B,C,D,E,F,G) :- H=1+B, I=1+E, A>=1, new7(A,H,C,B,I,F,G).
new7(A,B,C,D,E,F,G) :- A=0, new11(A,B,C,D,E,F,G).
new6(A,B,C,D,E,F,G) :- H=0, B-C>=1, new7(A,H,B,D,E,F,G).
new6(A,B,C,D,E,F,G) :- H=0, B-C=<0, new7(A,H,C,D,E,F,G).
new3(A,B,C,D,E,F,G) :- H=1+B, A=< -1, new3(A,H,C,D,E,F,G).
new3(A,B,C,D,E,F,G) :- H=1+B, A>=1, new3(A,H,C,D,E,F,G).
new3(A,B,C,D,E,F,G) :- A=0, new6(A,B,C,D,E,F,G).
new2(A) :- B=0, C=0, new3(A,B,D,E,C,F,G).
new1 :- new2(A).
false :- new1.
