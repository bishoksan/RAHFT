new8(A,B,C,D,E) :- A=0.
new8(A,B,C,D,E) :- F=1+B, A=< -1, new3(F,C,D,E).
new8(A,B,C,D,E) :- F=1+B, A>=1, new3(F,C,D,E).
new6(A,B,C,D) :- E=1, C=<0, new8(E,A,B,C,D).
new6(A,B,C,D) :- E=0, C>=1, new8(E,A,B,C,D).
new5(A,B,C,D) :- D>=1, new6(A,B,C,D).
new5(A,B,C,D) :- E=1+A, F=1, D=<0, new3(E,B,F,D).
new4(A,B,C,D) :- new5(A,B,C,E).
new3(A,B,C,D) :- A-B=< -1, new4(A,B,C,D).
new2 :- A=0, B=0, new3(A,C,B,D).
new1 :- new2.
false :- new1.
