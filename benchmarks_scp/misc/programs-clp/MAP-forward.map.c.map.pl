new11(A,B,C,D,E,F) :- G=1+B, H=1+D, I=2+E, F=< -1, new7(A,G,C,H,I,F).
new11(A,B,C,D,E,F) :- G=1+B, H=1+D, I=2+E, F>=1, new7(A,G,C,H,I,F).
new11(A,B,C,D,E,F) :- G=1+B, H=2+D, I=1+E, F=0, new7(A,G,C,H,I,F).
new10(A,B,C,D,E,F) :- E+D-3*C=< -1.
new10(A,B,C,D,E,F) :- E+D-3*C>=1.
new9(A,B,C,D,E,F) :- new11(A,B,C,D,E,G).
new7(A,B,C,D,E,F) :- B-C=< -1, new9(A,B,C,D,E,F).
new7(A,B,C,D,E,F) :- B-C>=0, new10(A,B,C,D,E,F).
new6(A,B,C,D,E,F,G) :- new6(A,B,C,D,E,F,G).
new4(A,B,C,D,E,F,G) :- A=0, new6(A,B,C,D,E,F,G).
new4(A,B,C,D,E,F,G) :- H=0, I=0, J=0, A=< -1, new7(B,H,D,I,J,G).
new4(A,B,C,D,E,F,G) :- H=0, I=0, J=0, A>=1, new7(B,H,D,I,J,G).
new3(A,B,C,D,E,F) :- G=1, C>=0, new4(G,A,B,C,D,E,F).
new3(A,B,C,D,E,F) :- G=0, C=< -1, new4(G,A,B,C,D,E,F).
new2 :- new3(A,B,C,D,E,F).
new1 :- new2.
false :- new1.
