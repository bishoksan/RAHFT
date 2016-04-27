new8(A,B,C,D,E) :- F=B+D, B>=1, new3(A,B,C,F,E).
new8(A,B,C,D,E) :- F=1+D, B=<0, new3(A,B,C,F,E).
new6(A,B,C,D,E) :- D=<0.
new6(A,B,C,D,E) :- F= -1+D, G=1+E, D>=1, new5(A,B,C,F,G).
new5(A,B,C,D,E) :- A-E>=1, new6(A,B,C,D,E).
new4(A,B,C,D,E) :- new8(A,F,C,D,E).
new3(A,B,C,D,E) :- F=1+C, A-C>=1, new4(A,B,F,D,E).
new3(A,B,C,D,E) :- F=0, A-C=<0, new5(A,B,C,D,F).
new2 :- A=0, B=0, new3(C,D,A,B,E).
new1 :- new2.
false :- new1.
