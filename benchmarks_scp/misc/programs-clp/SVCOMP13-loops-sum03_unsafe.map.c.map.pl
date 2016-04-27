new18(A,B,C,D,E,F,G,H):-A=0.
new18(A,B,C,D,E,F,G,H):-A=< -1, new5(B,C,D,E,F,G,H).
new18(A,B,C,D,E,F,G,H):-A>=1, new5(B,C,D,E,F,G,H).
new11(A,B,C,D,E,F,G):-H=1, A=0, new9(A,B,C,D,E,F,H).
new11(A,B,C,D,E,F,G):-H=0, A=< -1, new9(A,B,C,D,E,F,H).
new11(A,B,C,D,E,F,G):-H=0, A>=1, new9(A,B,C,D,E,F,H).
new9(A,B,C,D,E,F,G):-H=:=G, new18(H,A,B,C,D,E,F,G).
new7(A,B,C,D,E,F,G):-H=1, A=4294967296-2*F, 2*F>=4294967297, new9(A,B,C,D,E,F,H).
new7(A,B,C,D,E,F,G):-H=1, A=:=2*F, F>=0, new9(A,B,C,D,E,F,H).
new7(A,B,C,D,E,F,G):-A=< -1, A+2*F>=4294967297, new11(A,B,C,D,E,F,G).
new7(A,B,C,D,E,F,G):-A-2*F=< -1, A>=0, new11(A,B,C,D,E,F,G).
new7(A,B,C,D,E,F,G):-A=< -1, A+2*F=<4294967295, F>=0, new11(A,B,C,D,E,F,G).
new7(A,B,C,D,E,F,G):-F>=0, A-2*F>=1, new11(A,B,C,D,E,F,G).
new6(A,B,C,D,E,F,G):-H=1+F, I=2+A, F=<9, F>=0, new7(I,B,C,D,E,H,G).
new6(A,B,C,D,E,F,G):-H=1+F, F>=10, new7(A,B,C,D,E,H,G).
new5(A,B,C,D,E,F,G):-new6(A,B,C,D,E,F,G).
new4(A,B,C,D,E,F,G):-H=0, I=:=J, J>=0, new5(A,B,C,J,I,H,G).
new3(A,B,C,D,E,F,G):-H=:=I, I>=0, new4(A,I,H,D,E,F,G).
new2:-A=0, new3(A,B,C,D,E,F,G).
new1:-new2.
false:-new1.
