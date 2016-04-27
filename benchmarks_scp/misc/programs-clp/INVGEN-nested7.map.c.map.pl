new23(A,B,C,D,E,F,G) :- H=1+C, I=1+E, E-F=< -1, new23(A,B,H,D,I,F,G).
new23(A,B,C,D,E,F,G) :- H=1+B, E-F>=0, new5(A,H,C,D,E,F,G).
new19(A,B,C,D,E,F,G) :- H=1+C, C-D-F=< -1, new19(A,B,H,D,E,F,G).
new19(A,B,C,D,E,F,G) :- H=1+E, C-D-F>=0, new18(A,B,C,D,H,F,G).
new18(A,B,C,D,E,F,G) :- H=0, E-G=< -1, new19(A,B,H,D,E,F,G).
new18(A,B,C,D,E,F,G) :- H=1+B, E-G>=0, new4(A,H,C,D,E,F,G).
new13(A,B,C,D,E,F,G) :- H= -1+C, I=1+E, E-F=< -1, new13(A,B,H,D,I,F,G).
new13(A,B,C,D,E,F,G) :- H=1+B, E-F>=0, new4(A,H,C,D,E,F,G).
new12(A,B,C,D,E,F,G) :- H=0, D-F>=1, new13(A,B,C,D,H,F,G).
new12(A,B,C,D,E,F,G) :- H=1+B, D-F=<0, new4(A,H,C,D,E,F,G).
new11(A,B,C,D,E,F,G) :- H=1+E, E-G=< -1, new11(A,B,C,D,H,F,G).
new11(A,B,C,D,E,F,G) :- H=0, E-G>=0, new18(A,B,C,D,H,F,G).
new10(A,B,C,D,E,F,G) :- H=0, D>=6, new11(A,B,C,D,H,F,G).
new10(A,B,C,D,E,F,G) :- D=<5, new12(A,B,C,D,E,F,G).
new9(A,B,C,D,E,F,G) :- H=0, A=< -1, new23(A,B,C,D,H,F,G).
new9(A,B,C,D,E,F,G) :- H=0, A>=1, new23(A,B,C,D,H,F,G).
new9(A,B,C,D,E,F,G) :- H=1+B, A=0, new5(A,H,C,D,E,F,G).
new7(A,B,C,D,E,F,G,H) :- B=0.
new6(A,B,C,D,E,F,G) :- H=1, C-D-F=<0, new7(A,H,B,C,D,E,F,G).
new6(A,B,C,D,E,F,G) :- H=0, C-D-F>=1, new7(A,H,B,C,D,E,F,G).
new5(A,B,C,D,E,F,G) :- B-G=< -1, new9(A,B,D,D,E,F,G).
new5(A,B,C,D,E,F,G) :- B-G>=0, new10(A,B,C,D,E,F,G).
new4(A,B,C,D,E,F,G) :- H=0, B-F=< -1, new5(A,H,C,D,E,F,G).
new4(A,B,C,D,E,F,G) :- B-F>=0, new6(A,B,C,D,E,F,G).
new3(A,B,C,D,E,F,G) :- H=0, C-D-F=<0, new4(A,H,C,D,E,F,G).
new2(A) :- new3(A,B,C,D,E,F,G).
new1 :- new2(A).
false :- new1.
