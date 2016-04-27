new6(A,B,C,D) :- B=0.
new5(A,B,C) :- D=1, C>=1, new6(A,D,B,C).
new5(A,B,C) :- D=0, C=<0, new6(A,D,B,C).
new3(A,B,C) :- D=B+C, E=1+C, B=< -1, new3(A,D,E).
new3(A,B,C) :- B>=0, new5(A,B,C).
new2(A) :- B= -50, new3(A,B,C).
new1 :- new2(A).
false :- new1.
