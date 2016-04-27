new11(A,B,C,D,E) :- B=0.
new9(A,B,C,D) :- E=1, C-D=< -1, new11(A,E,B,C,D).
new9(A,B,C,D) :- E=0, C-D>=0, new11(A,E,B,C,D).
new7(A,B,C,D,E) :- B=0.
new7(A,B,C,D,E) :- B=< -1, new9(A,C,D,E).
new7(A,B,C,D,E) :- B>=1, new9(A,C,D,E).
new6(A,B,C,D) :- E=1, C>=0, new7(A,E,B,C,D).
new6(A,B,C,D) :- E=0, C=< -1, new7(A,E,B,C,D).
new5(A,B,C,D) :- D>=1, new6(A,B,C,D).
new4(A,B,C,D) :- E=1+B, A=< -1, new3(A,E,B,D).
new4(A,B,C,D) :- E=1+B, A>=1, new3(A,E,B,D).
new4(A,B,C,D) :- E=1+B, A=0, new3(A,E,C,D).
new3(A,B,C,D) :- B-D=< -1, new4(A,B,C,D).
new3(A,B,C,D) :- B-D>=0, new5(A,B,C,D).
new2(A) :- B=0, C=0, new3(A,B,C,D).
new1 :- new2(A).
false :- new1.
