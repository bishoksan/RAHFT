new10(A,B,C,D,E,F,G) :- B=0.
new10(A,B,C,D,E,F,G) :- H=1+E, B=< -1, new6(A,C,D,H,F,G).
new10(A,B,C,D,E,F,G) :- H=1+E, B>=1, new6(A,C,D,H,F,G).
new8(A,B,C,D,E,F) :- G=1, B+C-D-E-F=<0, new10(A,G,B,C,D,E,F).
new8(A,B,C,D,E,F) :- G=0, B+C-D-E-F>=1, new10(A,G,B,C,D,E,F).
new6(A,B,C,D,E,F) :- D-E-F=< -1, new8(A,B,C,D,E,F).
new6(A,B,C,D,E,F) :- G=1+C, D-E-F>=0, new5(A,B,G,D,E,F).
new5(A,B,C,D,E,F) :- C-E=< -1, new6(A,B,C,C,E,F).
new5(A,B,C,D,E,F) :- G=1+B, C-E>=0, new4(A,G,C,D,E,F).
new4(A,B,C,D,E,F) :- G=0, B-E=< -1, new5(A,B,G,D,E,F).
new3(A,B,C,D,E,F) :- G=0, E-F=<0, new4(A,G,C,D,E,F).
new2(A) :- new3(A,B,C,D,E,F).
new1 :- new2(A).
false :- new1.
