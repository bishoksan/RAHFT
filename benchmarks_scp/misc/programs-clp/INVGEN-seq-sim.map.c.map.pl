new11(A,B,C,D,E) :- A=0.
new11(A,B,C,D,E) :- F=1+D, G= -1+E, A=< -1, new9(B,C,F,G).
new11(A,B,C,D,E) :- F=1+D, G= -1+E, A>=1, new9(B,C,F,G).
new10(A,B,C,D) :- E=1, D>=1, new11(E,A,B,C,D).
new10(A,B,C,D) :- E=0, D=<0, new11(E,A,B,C,D).
new9(A,B,C,D) :- A-C>=1, new10(A,B,C,D).
new7(A,B,C,D) :- E=1+C, F= -1+D, B-C>=1, new7(A,B,E,F).
new7(A,B,C,D) :- E=0, B-C=<0, new9(A,B,E,D).
new5(A,B,C,D) :- E=1+C, F=1+D, B-C>=1, new5(A,B,E,F).
new5(A,B,C,D) :- E=0, B-C=<0, new7(A,B,E,D).
new3(A,B,C,D) :- E=1+C, F=1+D, A-C>=1, new3(A,B,E,F).
new3(A,B,C,D) :- E=0, A-C=<0, new5(A,B,E,D).
new2 :- A=0, B=0, new3(C,D,A,B).
new1 :- new2.
false :- new1.
