new7(A,B,C,D,E) :- A=0.
new6(A,B,C,D) :- E=1, D=17, new7(E,A,B,C,D).
new6(A,B,C,D) :- E=0, D=<16, new7(E,A,B,C,D).
new6(A,B,C,D) :- E=0, D>=18, new7(E,A,B,C,D).
new5(A,B,C,D) :- E=4+C, new6(A,B,C,E).
new4(A,B,C,D) :- E=3+B, new5(A,B,E,D).
new3(A,B,C,D) :- E=2+A, new4(A,E,C,D).
new2 :- A=8, new3(A,B,C,D).
new1 :- new2.
false :- new1.
