new7(A,B,C,D,E) :- A=0.
new6(A,B,C,D) :- E=1, D>=0, A-D>=1, new7(E,A,B,C,D).
new6(A,B,C,D) :- E=1, A-D=< -1, A>=0, new7(E,A,B,C,D).
new6(A,B,C,A) :- D=0, A>=0, new7(D,A,B,C,A).
new4(A,B,C,D) :- E= -1+C, F=1+D, C>=1, D>=0, new4(A,B,E,F).
new4(A,B,C,D) :- C=0, new6(A,B,C,D).
new3(A,B,C,D) :- E=0, F>=0, new4(F,F,F,E).
new2 :- new3(A,B,C,D).
new1 :- new2.
false :- new1.
