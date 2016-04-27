new16(A,B,C,D,E) :- F= -(C), G=10+D, H=1+E, 100*B-1*D=<0, new3(A,B,F,G,H).
new16(A,B,C,D,E) :- F=10+D, G=1+E, 100*B-1*D>=1, new3(A,B,C,F,G).
new15(A,B,C,D,E) :- C-10*E>=1, new16(A,B,C,D,E).
new15(A,B,C,D,E) :- F=10+D, G=1+E, C-10*E=<0, new3(A,B,C,F,G).
new13(A,B,C,D,E) :- F=1+B, G=1+C, H=10+D, I=1+E, B>=4, new3(A,F,G,H,I).
new13(A,B,C,D,E) :- F=10+D, G=1+E, B=<3, new3(A,B,C,F,G).
new12(A,B,C,D,E) :- A=< -1, new13(A,B,C,D,E).
new12(A,B,C,D,E) :- A>=1, new13(A,B,C,D,E).
new12(A,B,C,D,E) :- A=0, new15(A,B,C,D,E).
new8(A,B,C,D,E,F) :- B=0.
new7(A,B,C,D,E) :- F=1, C>=3, new8(A,F,B,C,D,E).
new7(A,B,C,D,E) :- F=0, C=<2, new8(A,F,B,C,D,E).
new6(A,B,C,D,E) :- B>=4, new7(A,B,C,D,E).
new4(A,B,C,D,E) :- F=1+B, G=100+C, H=10+D, I=1+E, A=< -1, new3(A,F,G,H,I).
new4(A,B,C,D,E) :- F=1+B, G=100+C, H=10+D, I=1+E, A>=1, new3(A,F,G,H,I).
new4(A,B,C,D,E) :- A=0, new12(A,B,C,D,E).
new3(A,B,C,D,E) :- A=< -1, new4(A,B,C,D,E).
new3(A,B,C,D,E) :- A>=1, new4(A,B,C,D,E).
new3(A,B,C,D,E) :- A=0, new6(A,B,C,D,E).
new2(A) :- B=0, C=0, D=0, E=0, new3(A,B,C,D,E).
new1 :- new2(A).
false :- new1.
