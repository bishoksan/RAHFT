new13(A,B,C,D,E,F,G) :- B=0.
new13(A,B,C,D,E,F,G) :- H=1+D, I=1+E, B=< -1, new4(A,C,H,I,F,G).
new13(A,B,C,D,E,F,G) :- H=1+D, I=1+E, B>=1, new4(A,C,H,I,F,G).
new12(A,B,C,D,E,F) :- G=1, C>=0, new13(A,G,B,C,D,E,F).
new12(A,B,C,D,E,F) :- G=0, C=< -1, new13(A,G,B,C,D,E,F).
new11(A,B,C,D,E,F) :- A=< -1, new12(A,B,C,D,E,F).
new11(A,B,C,D,E,F) :- A>=1, new12(A,B,C,D,E,F).
new11(A,B,C,D,E,F) :- G=2+C, H=3+D, A=0, new4(A,B,G,H,E,F).
new9(A,B,C,D,E,F) :- new3(A,B,C,D,E,F).
new8(A,B,C,D,E,F) :- B-C>=2, new11(A,B,C,D,E,F).
new8(A,B,C,D,E,F) :- B-C=<1, new12(A,B,C,D,E,F).
new6(A,B,C,D,E,F) :- new3(A,B,C,D,E,F).
new5(A,B,C,D,E,F) :- D-F=< -1, new8(A,B,C,D,E,F).
new5(A,B,C,D,E,F) :- D-F>=0, new9(A,B,C,D,E,F).
new4(A,B,C,D,E,F) :- B-C>=1, new5(A,B,C,D,E,F).
new4(A,B,C,D,E,F) :- B-C=<0, new6(A,B,C,D,E,F).
new3(A,B,C,D,E,F) :- G=0, B-C>=1, new4(A,B,C,G,E,F).
new2(A) :- B=0, C=4+D, new3(A,E,B,F,C,D).
new1 :- new2(A).
false :- new1.
