new15(A,B) :- A=0.
new10(A,B,C) :- D=15, B=15, new6(D).
new10(A,B,C) :- D=20, B=<14, new6(D).
new10(A,B,C) :- D=20, B>=16, new6(D).
new7(A,B,C) :- D=5, B=5, new6(D).
new7(A,B,C) :- B=<4, new10(A,B,C).
new7(A,B,C) :- B>=6, new10(A,B,C).
new6(A) :- B=1, A=<14, new15(B,A).
new6(A) :- B=1, A>=16, new15(B,A).
new6(A) :- B=0, A=15, new15(B,A).
new5(A,B,C) :- D=0, B=0, new6(D).
new5(A,B,C) :- B=< -1, new7(A,B,C).
new5(A,B,C) :- B>=1, new7(A,B,C).
new4(A,B,C) :- new5(A,D,C).
new3(A) :- new4(B,C,A).
new2 :- new3(A).
new1 :- new2.
false :- new1.
