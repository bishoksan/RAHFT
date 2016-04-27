new10(A,B,C,D,E,F,G,H) :- B=0.
new10(A,B,C,D,E,F,G,H) :- I=1+E, B=< -1, new6(A,C,D,I,F,G,H).
new10(A,B,C,D,E,F,G,H) :- I=1+E, B>=1, new6(A,C,D,I,F,G,H).
new8(A,B,C,D,E,F,G) :- H=1, B-D+2*E>=0, new10(A,H,B,C,D,E,F,G).
new8(A,B,C,D,E,F,G) :- H=0, B-D+2*E=< -1, new10(A,H,B,C,D,E,F,G).
new6(A,B,C,D,E,F,G) :- C-D>=1, new8(A,B,C,D,E,F,G).
new6(A,B,C,D,E,F,G) :- H=1+C, C-D=<0, new5(A,B,H,D,E,F,G).
new5(A,B,C,D,E,F,G) :- 3*B-1*C>=1, new6(A,B,C,B,E,F,G).
new5(A,B,C,D,E,F,G) :- H=1+B, 3*B-1*C=<0, new4(A,H,C,D,E,F,G).
new4(A,B,C,D,E,F,G) :- H=2*B, B-E=< -1, new5(A,B,H,D,E,F,G).
new3(A,B,C,D,E,F,G) :- H=0, 3*E-1*F-1*G=<0, new4(A,H,C,D,E,F,G).
new2(A) :- new3(A,B,C,D,E,F,G).
new1 :- new2(A).
false :- new1.
