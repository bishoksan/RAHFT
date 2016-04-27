new10(A,B,C,D) :- E=0, F=1+C, D=< -1, new3(E,B,F,D).
new10(A,B,C,D) :- E=0, F=1+C, D>=1, new3(E,B,F,D).
new10(A,B,C,D) :- D=0, new3(A,B,C,D).
new7(A,B,C,D,E) :- A=0.
new6(A,B,C,D) :- E=1, A=< -1, new7(E,A,B,C,D).
new6(A,B,C,D) :- E=1, A>=1, new7(E,A,B,C,D).
new6(A,B,C,D) :- E=0, A=0, new7(E,A,B,C,D).
new4(A,B,C,D) :- new10(A,B,C,E).
new3(A,B,C,D) :- E=1, B-C>=1, new4(E,C,C,D).
new3(A,B,C,D) :- E=1, B-C=< -1, new4(E,C,C,D).
new3(A,B,B,C) :- new6(A,B,B,C).
new2 :- A=0, B= -1+C, new3(A,B,C,D).
new1 :- new2.
false :- new1.
