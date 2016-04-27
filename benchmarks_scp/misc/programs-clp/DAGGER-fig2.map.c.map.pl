new18(A,B,C,D,E,F,G) :- H= -(A), I= -(B), B-D=< -1, new3(H,I,C,D,E,F,G).
new18(A,B,C,D,E,F,G) :- B-D>=0, new3(A,B,C,D,E,F,G).
new17(A,B,C,D,E,F,G) :- A-C>=0, new18(A,B,C,D,E,F,G).
new17(A,B,C,D,E,F,G) :- A-C=< -1, new3(A,B,C,D,E,F,G).
new15(A,B,C,D,E,F,G) :- H=1+A, I=3+B, J=10+C, K=10+D, A>=4, new3(H,I,J,K,E,F,G).
new15(A,B,C,D,E,F,G) :- A=<3, new3(A,B,C,D,E,F,G).
new14(A,B,C,D,E,F,G) :- E=< -1, new15(A,B,C,D,E,F,G).
new14(A,B,C,D,E,F,G) :- E>=1, new15(A,B,C,D,E,F,G).
new14(A,B,C,D,E,F,G) :- E=0, new17(A,B,C,D,E,F,G).
new13(A,B,C,D,E,F,G) :- new14(A,B,C,D,H,F,G).
new10(A,B,C,D,E,F,G) :- H=1+A, I=2+B, F=< -1, new3(H,I,C,D,E,F,G).
new10(A,B,C,D,E,F,G) :- H=1+A, I=2+B, F>=1, new3(H,I,C,D,E,F,G).
new10(A,B,C,D,E,F,G) :- F=0, new13(A,B,C,D,E,F,G).
new8(A,B,C,D,E,F,G,H) :- A=0.
new7(A,B,C,D,E,F,G) :- H=1, 3*A-1*B>=0, new8(H,A,B,C,D,E,F,G).
new7(A,B,C,D,E,F,G) :- H=0, 3*A-1*B=< -1, new8(H,A,B,C,D,E,F,G).
new5(A,B,C,D,E,F,G) :- new10(A,B,C,D,E,H,G).
new4(A,B,C,D,E,F,G) :- G=< -1, new5(A,B,C,D,E,F,G).
new4(A,B,C,D,E,F,G) :- G>=1, new5(A,B,C,D,E,F,G).
new4(A,B,C,D,E,F,G) :- G=0, new7(A,B,C,D,E,F,G).
new3(A,B,C,D,E,F,G) :- new4(A,B,C,D,E,F,H).
new2 :- A=0, B=0, C=0, D=0, new3(A,B,C,D,E,F,G).
new1 :- new2.
false :- new1.
