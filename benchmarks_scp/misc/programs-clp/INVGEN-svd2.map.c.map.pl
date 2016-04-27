new29(A,B,C,D,E,F,G) :- B=0.
new29(A,B,C,D,E,F,G) :- H=1+F, B=< -1, new22(A,C,D,E,H,G).
new29(A,B,C,D,E,F,G) :- H=1+F, B>=1, new22(A,C,D,E,H,G).
new27(A,B,C,D,E,F) :- G=1, B-C>=0, new29(A,G,B,C,D,E,F).
new27(A,B,C,D,E,F) :- G=0, B-C=< -1, new29(A,G,B,C,D,E,F).
new25(A,B,C,D,E,F,G) :- B=0.
new25(A,B,C,D,E,F,G) :- B=< -1, new27(A,C,D,E,F,G).
new25(A,B,C,D,E,F,G) :- B>=1, new27(A,C,D,E,F,G).
new23(A,B,C,D,E,F) :- G=1, C>=1, new25(A,G,B,C,D,E,F).
new23(A,B,C,D,E,F) :- G=0, C=<0, new25(A,G,B,C,D,E,F).
new22(A,B,C,D,E,F) :- B-E>=0, new23(A,B,C,D,E,F).
new22(A,B,C,D,E,F) :- G=1+D, B-E=< -1, new18(A,B,C,G,E,F).
new19(A,B,C,D,E,F) :- G=1+E, B-E>=0, new19(A,B,C,D,G,F).
new19(A,B,C,D,E,F) :- B-E=< -1, new22(A,B,C,D,F,F).
new18(A,B,C,D,E,F) :- B-D>=0, new19(A,B,C,D,F,F).
new18(A,B,C,D,E,F) :- B-D=< -1, new14(A,B,C,F,E,F).
new14(A,B,C,D,E,F) :- G=1+D, B-D>=0, new14(A,B,C,G,E,F).
new14(A,B,C,D,E,F) :- G= -1+C, B-D=< -1, new7(A,B,G,D,E,C).
new12(A,B,C,D,E,F) :- G=1+D, B-D>=0, new12(A,B,C,G,E,F).
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
new3(A,B,C,D,E,F) :- G=1, F>=1, new4(A,G,B,C,D,E,F).
new3(A,B,C,D,E,F) :- G=0, F=<0, new4(A,G,B,C,D,E,F).
new2(A) :- new3(A,B,C,D,E,F).
new1 :- new2(A).
false :- new1.
