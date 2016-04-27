new10(A,B,C,D,E,F) :- G=A+B, H= -(A)-2*B, I=3*A+B, E=< -1, new3(A,G,H,I,E,F).
new10(A,B,C,D,E,F) :- G=A+B, H= -(A)-2*B, I=3*A+B, E>=1, new3(A,G,H,I,E,F).
new10(A,B,C,D,E,F) :- G= -(A)+B, H=3*A-2*B, I=A+B, E=0, new3(A,G,H,I,E,F).
new8(A,B,C,D,E,F,G) :- A=0.
new7(A,B,C,D,E,F) :- G=1, C+2*D>=0, new8(G,A,B,C,D,E,F).
new7(A,B,C,D,E,F) :- G=0, C+2*D=< -1, new8(G,A,B,C,D,E,F).
new5(A,B,C,D,E,F) :- new10(A,B,C,D,G,F).
new4(A,B,C,D,E,F) :- G=1+C+2*D, H= -2*C+D, F=< -1, new5(G,H,C,D,E,F).
new4(A,B,C,D,E,F) :- G=1+C+2*D, H= -2*C+D, F>=1, new5(G,H,C,D,E,F).
new4(A,B,C,D,E,F) :- F=0, new7(A,B,C,D,E,F).
new3(A,B,C,D,E,F) :- new4(A,B,C,D,E,G).
new2 :- A=0, B=0, new3(C,D,A,B,E,F).
new1 :- new2.
false :- new1.
