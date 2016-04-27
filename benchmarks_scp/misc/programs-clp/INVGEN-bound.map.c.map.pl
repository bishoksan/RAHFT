new18(A,B,C,D,E,F,G) :- A=0.
new17(A,B,C,D,E,F) :- G=1, C>=0, new18(G,A,B,C,D,E,F).
new17(A,B,C,D,E,F) :- G=0, C=< -1, new18(G,A,B,C,D,E,F).
new15(A,B,C,D,E,F) :- G=D+F, H= -1+D, I=1+F, D>=1, new15(A,G,C,H,E,I).
new15(A,B,C,D,E,F) :- D=<0, new17(A,B,C,D,E,F).
new12(A,B,C,D,E,F) :- G=C+E, H= -1+C, I=1+E, C>=1, new12(A,G,H,D,I,F).
new12(A,B,C,D,E,F) :- G=0, C=<0, new15(A,E,C,E,E,G).
new11(A,B,C,D,E,F,G) :- new11(A,B,C,D,E,F,G).
new9(A,B,C,D,E,F,G) :- A=0, new11(A,B,C,D,E,F,G).
new9(A,B,C,D,E,F,G) :- H=0, A=< -1, new12(B,B,B,E,H,G).
new9(A,B,C,D,E,F,G) :- H=0, A>=1, new12(B,B,B,E,H,G).
new7(A,B,C,D,E,F) :- G=1, A=<200, new9(G,A,B,C,D,E,F).
new7(A,B,C,D,E,F) :- G=0, A>=201, new9(G,A,B,C,D,E,F).
new6(A,B,C,D,E,F,G) :- new6(A,B,C,D,E,F,G).
new4(A,B,C,D,E,F,G) :- A=0, new6(A,B,C,D,E,F,G).
new4(A,B,C,D,E,F,G) :- A=< -1, new7(B,C,D,E,F,G).
new4(A,B,C,D,E,F,G) :- A>=1, new7(B,C,D,E,F,G).
new3(A,B,C,D,E,F) :- G=1, A>=0, new4(G,A,B,C,D,E,F).
new3(A,B,C,D,E,F) :- G=0, A=< -1, new4(G,A,B,C,D,E,F).
new2 :- new3(A,B,C,D,E,F).
new1 :- new2.
false :- new1.
