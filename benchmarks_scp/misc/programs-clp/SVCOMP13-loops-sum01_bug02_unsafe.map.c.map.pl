new13(A,B,C,D,E,F,G):-A=0.
new8(A,B,C,D,E,F):-G=1, E=0, new7(A,B,C,D,E,G).
new8(A,B,C,D,E,F):-G=0, E=< -1, new7(A,B,C,D,E,G).
new8(A,B,C,D,E,F):-G=0, E>=1, new7(A,B,C,D,E,G).
new7(A,B,C,D,E,F):-G=:=F, new13(G,A,B,C,D,E,F).
new6(A,B,C,D,E,F):-G=1, 2*C=:=E, new7(A,B,C,D,E,G).
new6(A,B,C,D,E,F):- 2*C-E>=1, new8(A,B,C,D,E,F).
new6(A,B,C,D,E,F):- 2*C-E=< -1, new8(A,B,C,D,E,F).
new5(A,B,C,D,E,F):-G=1+A, H= -1+B, I=2+E, A-B=< -1, new4(G,H,C,D,I,F).
new5(A,B,C,D,E,F):-G=1+A, H= -1+B, A-B>=0, new4(G,H,C,D,E,F).
new4(A,B,C,D,E,F):-A-C=<0, new5(A,B,C,D,E,F).
new4(A,B,C,D,E,F):-A-C>=1, new6(A,B,C,D,E,F).
new3(A,B,C,D,E,F):-G=1, H=:=I, J=0, I>=0, new4(G,B,I,H,J,F).
new2:-A=10, new3(B,A,C,D,E,F).
new1:-new2.
false:-new1.
