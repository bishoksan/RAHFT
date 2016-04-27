new14(A,B,C,D,E,F) :- G=0, B=< -1, new11(A,B,C,D,E,G).
new14(A,B,C,D,E,F) :- G=1, B>=0, new11(A,B,C,D,E,G).
new12(A,B,C,D,E,F,G) :- A=0.
new11(A,B,C,D,E,F) :- new12(F,A,B,C,D,E,F).
new10(A,B,C,D,E,F) :- G=0, A=< -1, new11(A,B,C,D,E,G).
new10(A,B,C,D,E,F) :- A>=0, new14(A,B,C,D,E,F).
new8(A,B,C,D,E,F) :- C>=7, new10(A,B,C,D,E,F).
new8(A,B,C,D,E,F) :- G=1, C=<6, new11(A,B,C,D,E,G).
new6(A,B,C,D,E,F) :- G=4+C, B>=1, new8(A,B,G,D,E,F).
new6(A,B,C,D,E,F) :- G=3, B=<0, new8(A,G,C,D,E,F).
new4(A,B,C,D,E,F) :- G=3, D>=1, new6(A,B,C,D,G,F).
new4(A,B,C,D,E,F) :- G=2, D=<0, new6(A,B,C,D,G,F).
new3(A,B,C,D,E,F) :- G=2+C, A>=1, new4(A,B,G,D,E,F).
new3(A,B,C,D,E,F) :- G=3, A=<0, new4(G,B,C,D,E,F).
new2 :- A=0, new3(B,C,A,D,E,F).
new1 :- new2.
false :- new1.
