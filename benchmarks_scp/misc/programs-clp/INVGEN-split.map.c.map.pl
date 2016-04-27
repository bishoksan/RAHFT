new6(A,B,C,D,E,F) :- A=0.
new5(A,B,C,C,D) :- E=1, new6(E,A,B,C,C,D).
new5(A,B,C,D,E) :- F=0, C-D=< -1, new6(F,A,B,C,D,E).
new5(A,B,C,D,E) :- F=0, C-D>=1, new6(F,A,B,C,D,E).
new4(A,B,C,D,E) :- F=0, G=1+C, H=1+E, B=< -1, new3(A,F,G,D,H).
new4(A,B,C,D,E) :- F=0, G=1+C, H=1+E, B>=1, new3(A,F,G,D,H).
new4(A,B,C,D,E) :- F=1, G=1+D, H=1+E, B=0, new3(A,F,C,G,H).
new3(A,B,C,D,E) :- 2*A-1*E>=1, new4(A,B,C,D,E).
new3(A,B,C,D,E) :- 2*A-1*E=<0, new5(A,B,C,D,E).
new2 :- A=100, B=0, new3(A,C,D,D,B).
new1 :- new2.
false :- new1.
