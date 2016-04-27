new6(A,B,C,D) :- C=<0.
new6(A,B,C,D) :- E= -1+C, F=2+D, C>=1, new5(A,B,E,F).
new5(A,B,C,D) :- A-D>=1, new6(A,B,C,D).
new3(A,B,C,D) :- E=1+B, F=2+C, A-B>=1, new3(A,E,F,D).
new3(A,B,C,D) :- E=0, A-B=<0, new5(A,B,C,E).
new2 :- A=0, B=0, new3(C,A,B,D).
new1 :- new2.
false :- new1.
