new23(A,B,C,D,E,F,G) :- B=0.
new23(A,B,C,D,E,F,G) :- H=1+E, B=< -1, new12(A,C,D,H,F,G).
new23(A,B,C,D,E,F,G) :- H=1+E, B>=1, new12(A,C,D,H,F,G).
new19(A,B,C,D,E,F) :- G=1+E, B-E>=0, new19(A,B,C,D,G,F).
new19(A,B,C,D,E,F) :- G=1+D, B-E=< -1, new18(A,B,C,G,E,F).
new18(A,B,C,D,E,F) :- B-D>=0, new19(A,B,C,D,F,F).
new18(A,B,C,D,E,F) :- B-D=< -1, new14(A,B,C,F,E,F).
new17(A,B,C,D,E,F) :- G=1, D>=1, new23(A,G,B,C,D,E,F).
new17(A,B,C,D,E,F) :- G=0, D=<0, new23(A,G,B,C,D,E,F).
new14(A,B,C,D,E,F) :- G=1+D, B-D>=0, new14(A,B,C,G,E,F).
new14(A,B,C,D,E,F) :- G= -1+C, B-D=< -1, new7(A,B,G,D,E,C).
new12(A,B,C,D,E,F) :- B-D>=0, new17(A,B,C,D,E,F).
new12(A,B,C,D,E,F) :- B-D=< -1, new18(A,B,C,F,E,F).
new10(A,B,C,D,E,F) :- A=< -1, new12(A,B,C,F,E,F).
new10(A,B,C,D,E,F) :- A>=1, new12(A,B,C,F,E,F).
new10(A,B,C,D,E,F) :- A=0, new14(A,B,C,F,E,F).
new9(A,B,C,D,E,F) :- B-C>=1, new10(A,B,C,D,E,F).
new9(A,B,C,D,E,F) :- G= -1+C, B-C=<0, new7(A,B,G,D,E,C).
new7(A,B,C,D,E,F) :- C>=1, new9(A,B,C,D,E,F).
new6(A,B,C,D,E,F,G) :- new6(A,B,C,D,E,F,G).
new4(A,B,C,D,E,F,G) :- B=0, new6(A,B,C,D,E,F,G).
new4(A,B,C,D,E,F,G) :- B=< -1, new7(A,C,C,E,F,G).
new4(A,B,C,D,E,F,G) :- B>=1, new7(A,C,C,E,F,G).
new3(A,B,C,D,E,F) :- G=1, F>=2, new4(A,G,B,C,D,E,F).
new3(A,B,C,D,E,F) :- G=0, F=<1, new4(A,G,B,C,D,E,F).
new2(A) :- new3(A,B,C,D,E,F).
new1 :- new2(A).
false :- new1.
