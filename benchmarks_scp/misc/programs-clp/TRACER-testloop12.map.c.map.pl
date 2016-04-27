new8(A,B,C,D,E,F) :- B=0.
new8(A,B,C,D,E,F) :- G=1+E, B=< -1, new3(A,G,F).
new8(A,B,C,D,E,F) :- G=1+E, B>=1, new3(A,G,F).
new6(A,B,C,D,E) :- F=1, A>=0, new8(A,F,B,C,D,E).
new6(A,B,C,D,E) :- F=0, A=< -1, new8(A,F,B,C,D,E).
new5(A,B,C,D,E) :- F=1, B>=0, new6(A,B,F,D,E).
new5(A,B,C,D,E) :- F=1, G=1+D, B=< -1, new3(F,G,E).
new4(A,B,C) :- new5(A,C,D,B,C).
new3(A,B,C) :- B=<9, new4(A,B,C).
new2(A) :- B=0, C=0, new3(B,C,D).
new1 :- new2(A).
false :- new1.
