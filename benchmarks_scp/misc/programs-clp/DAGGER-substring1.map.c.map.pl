new10(A,B,C,D,E) :- A=0.
new9(A,B,C,D) :- E=1, B=<100, new10(E,A,B,C,D).
new9(A,B,C,D) :- E=0, B>=101, new10(E,A,B,C,D).
new7(A,B,C,D) :- E=1+A, F=1+B, A-D=< -1, new7(E,F,C,D).
new7(A,B,C,D) :- A-D>=0, new9(A,B,C,D).
new6(A,B,C,D) :- E=0, C-D=<0, new7(C,E,C,D).
new5(A,B,C,D) :- C>=0, new6(A,B,C,D).
new4(A,B,C,D) :- D=<100, new5(A,B,C,D).
new3(A,B,C,D) :- D>=0, new4(A,B,C,D).
new2 :- new3(A,B,C,D).
new1 :- new2.
false :- new1.
