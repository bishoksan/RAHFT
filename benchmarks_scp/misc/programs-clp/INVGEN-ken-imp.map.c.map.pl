new7(A,B,C,D) :- D=< -1.
new7(A,B,C,D) :- D>=1.
new6(A,A,B,C) :- new7(A,A,B,C).
new3(A,B,C,D) :- E= -1+C, F= -1+D, C=< -1, new3(A,B,E,F).
new3(A,B,C,D) :- E= -1+C, F= -1+D, C>=1, new3(A,B,E,F).
new3(A,B,C,D) :- C=0, new6(A,B,C,D).
new2 :- new3(A,B,A,B).
new1 :- new2.
false :- new1.
