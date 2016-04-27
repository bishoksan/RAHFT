new6(A,B,C,D) :- A=0.
new5(A,B,C) :- D=1, C=<2, new6(D,A,B,C).
new5(A,B,C) :- D=0, C>=3, new6(D,A,B,C).
new4(A,B,C) :- D=1+B, E=2, C=1, new3(A,D,E).
new4(A,B,C) :- D=1+B, E=1, C=<0, new3(A,D,E).
new4(A,B,C) :- D=1+B, E=1, C>=2, new3(A,D,E).
new3(A,B,C) :- A-B>=1, new4(A,B,C).
new3(A,B,C) :- A-B=<0, new5(A,B,C).
new2 :- A=0, B=1, new3(C,A,B).
new1 :- new2.
false :- new1.
