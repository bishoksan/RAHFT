new8(A,B,C,D,E,F) :- D=<0.
new8(A,B,C,D,E,F) :- G= -1+D, H=1+F, D>=1, new7(A,B,C,G,E,H).
new7(A,B,C,D,E,F) :- A+B-F>=1, new8(A,B,C,D,E,F).
new5(A,B,C,D,E,F) :- G=1+D, H=1+E, B-E>=1, new5(A,B,C,G,H,F).
new5(A,B,C,D,E,F) :- G=0, B-E=<0, new7(A,B,C,D,E,G).
new3(A,B,C,D,E,F) :- G=1+C, H=1+D, A-C>=1, new3(A,B,G,H,E,F).
new3(A,B,C,D,E,F) :- G=0, A-C=<0, new5(A,B,C,D,G,F).
new2 :- A=0, B=0, new3(C,D,A,B,E,F).
new1 :- new2.
false :- new1.
