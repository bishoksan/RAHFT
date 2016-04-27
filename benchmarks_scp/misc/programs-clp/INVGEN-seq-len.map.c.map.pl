new15(A,B,C,D,E,F) :- A=0.
new15(A,B,C,D,E,F) :- G=1+E, H= -1+F, A=< -1, new13(B,C,D,G,H).
new15(A,B,C,D,E,F) :- G=1+E, H= -1+F, A>=1, new13(B,C,D,G,H).
new14(A,B,C,D,E) :- F=1, E>=1, new15(F,A,B,C,D,E).
new14(A,B,C,D,E) :- F=0, E=<0, new15(F,A,B,C,D,E).
new13(A,B,C,D,E) :- A-D>=1, new14(A,B,C,D,E).
new11(A,B,C,D,E) :- F=1+D, G= -1+E, B-D>=1, new11(A,B,C,F,G).
new11(A,B,C,D,E) :- F=0, B-D=<0, new13(A,B,C,F,E).
new9(A,B,C,D,E) :- F=1+D, G= -1+E, C-D>=1, new9(A,B,C,F,G).
new9(A,B,C,D,E) :- F=0, C-D=<0, new11(A,B,C,F,E).
new7(A,B,C,D,E) :- F=1+D, G=1+E, C-D>=1, new7(A,B,C,F,G).
new7(A,B,C,D,E) :- F=0, C-D=<0, new9(A,B,C,F,E).
new5(A,B,C,D,E) :- F=1+D, G=1+E, B-D>=1, new5(A,B,C,F,G).
new5(A,B,C,D,E) :- F=0, B-D=<0, new7(A,B,C,F,E).
new3(A,B,C,D,E) :- F=1+D, G=1+E, A-D>=1, new3(A,B,C,F,G).
new3(A,B,C,D,E) :- F=0, A-D=<0, new5(A,B,C,F,E).
new2 :- A=0, B=0, new3(C,D,E,A,B).
new1 :- new2.
false :- new1.
