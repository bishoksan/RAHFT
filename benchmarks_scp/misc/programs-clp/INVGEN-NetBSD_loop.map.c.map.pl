new10(A,B,C,D,E,F,G,H) :- B=0.
new10(A,B,C,D,E,F,G,H) :- I=1+F, B=< -1, new4(A,C,D,E,I,G,H).
new10(A,B,C,D,E,F,G,H) :- I=1+F, B>=1, new4(A,C,D,E,I,G,H).
new8(A,B,C,D,E,F,G) :- H=1, B-E>=0, new10(A,H,B,C,D,E,F,G).
new8(A,B,C,D,E,F,G) :- H=0, B-E=< -1, new10(A,H,B,C,D,E,F,G).
new6(A,B,C,D,E,F,G,H) :- B=0.
new6(A,B,C,D,E,F,G,H) :- B=< -1, new8(A,C,D,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- B>=1, new8(A,C,D,E,F,G,H).
new5(A,B,C,D,E,F,G) :- H=1, E>=0, new6(A,H,B,C,D,E,F,G).
new5(A,B,C,D,E,F,G) :- H=0, E=< -1, new6(A,H,B,C,D,E,F,G).
new4(A,B,C,D,E,F,G) :- E-G=<0, new5(A,B,C,D,E,F,G).
new3(A,B,C,D,E,F,G) :- H=0, I=0, J=0, B>=1, new4(A,B,H,B,I,J,B).
new2(A) :- new3(A,B,C,D,E,F,G).
new1 :- new2(A).
false :- new1.
