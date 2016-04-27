new8(A,B,C,D,E) :- A=0.
new7(A,B,C,D) :- E=1, C=< -1, new8(E,A,B,C,D).
new7(A,B,C,D) :- E=1, C>=1, new8(E,A,B,C,D).
new7(A,B,C,D) :- E=0, C=0, new8(E,A,B,C,D).
new6(A,B,C,D) :- E=0, F=5+A, new7(A,F,E,D).
new4(A,B,C,D) :- E=0, F=3+A, new7(A,F,E,D).
new3(A,B,C,D) :- D=< -1, new4(A,B,C,D).
new3(A,B,C,D) :- D>=1, new4(A,B,C,D).
new3(A,B,C,D) :- D=0, new6(A,B,C,D).
new2 :- new3(A,B,C,D).
new1 :- new2.
false :- new1.
