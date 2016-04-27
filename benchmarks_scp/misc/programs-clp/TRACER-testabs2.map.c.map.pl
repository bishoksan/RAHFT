new6(A,B,C,D,E) :- B=0.
new4(A,B,C,D) :- E=1, B>=1, new6(A,E,B,C,D).
new4(A,B,C,D) :- E=0, B=<0, new6(A,E,B,C,D).
new3(A,B,C,D) :- E=1, F=4, D>=1, new4(A,F,E,D).
new3(A,B,C,D) :- E=2, F=100, D=<0, new4(A,F,E,D).
new2(A) :- B=4, C=1, new3(A,B,D,C).
new1 :- A=0, new2(A).
false :- new1.
