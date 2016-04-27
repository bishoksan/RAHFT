new7(A,B,C,D,E,F) :- A=0.
new6(A,B,C,D,E) :- F=1, A=1/3*C+1/3*D, new7(F,A,B,C,D,E).
new6(A,B,C,D,E) :- F=0, A-1/3*C-1/3*D>=1/3, new7(F,A,B,C,D,E).
new6(A,B,C,D,E) :- F=0, A-1/3*C-1/3*D=< -1/3, new7(F,A,B,C,D,E).
new5(A,B,C,D,E) :- F=1+B, G=1+C, H=2+D, E=< -1, new4(A,F,G,H,E).
new5(A,B,C,D,E) :- F=1+B, G=1+C, H=2+D, E>=1, new4(A,F,G,H,E).
new5(A,B,C,D,E) :- F=1+B, G=2+C, H=1+D, E=0, new4(A,F,G,H,E).
new4(A,B,C,D,E) :- A-B>=1, new5(A,B,C,D,E).
new4(A,B,C,D,E) :- A-B=<0, new6(A,B,C,D,E).
new3(A,B,C,D,E) :- F=0, G=0, H=0, A>=0, new4(A,F,G,H,E).
new2 :- new3(A,B,C,D,E).
new1 :- new2.
false :- new1.
