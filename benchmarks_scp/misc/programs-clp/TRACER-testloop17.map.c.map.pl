new11(A,B,C,D,E) :- A=0.
new10(A,B,C,D) :- E=1, D=<0, new11(E,A,B,C,D).
new10(A,B,C,D) :- E=0, D>=1, new11(E,A,B,C,D).
new9(A,B,C,D) :- E=1+B, F=1+C, G=1, B=<0, new7(A,E,F,G).
new9(A,B,C,D) :- E=1+B, F=1+C, G=0, B>=1, new7(A,E,F,G).
new7(A,B,C,D) :- A-B>=1, new9(A,B,C,D).
new7(A,B,C,D) :- A-B=<0, new10(A,B,C,D).
new6(A,B,C,D,E) :- new6(A,B,C,D,E).
new4(A,B,C,D,E) :- A=0, new6(A,B,C,D,E).
new4(A,B,C,D,E) :- A=< -1, new7(B,C,D,E).
new4(A,B,C,D,E) :- A>=1, new7(B,C,D,E).
new3(A,B,C,D) :- E=1, A>=2, new4(E,A,B,C,D).
new3(A,B,C,D) :- E=0, A=<1, new4(E,A,B,C,D).
new2 :- A=0, B=0, C=0, new3(D,B,C,A).
new1 :- new2.
false :- new1.
