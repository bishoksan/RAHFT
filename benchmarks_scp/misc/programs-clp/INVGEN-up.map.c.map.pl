new7(A,B,C,D,E) :- A=0.
new7(A,B,C,D,E) :- F= -1+D, G=1+E, A=< -1, new5(B,C,F,G).
new7(A,B,C,D,E) :- F= -1+D, G=1+E, A>=1, new5(B,C,F,G).
new6(A,B,C,D) :- E=1, C>=1, new7(E,A,B,C,D).
new6(A,B,C,D) :- E=0, C=<0, new7(E,A,B,C,D).
new5(A,B,C,D) :- A-D>=1, new6(A,B,C,D).
new3(A,B,C,D) :- E=1+B, F=1+C, A-B>=1, new3(A,E,F,D).
new3(A,B,C,D) :- E=0, A-B=<0, new5(A,B,C,E).
new2 :- A=0, B=0, new3(C,A,B,D).
new1 :- new2.
false :- new1.
