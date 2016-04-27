new12(A,B,C,D) :- A=0.
new7(A,B,C) :- D=1, B=0, new6(A,B,D).
new7(A,B,C) :- D=0, B=< -1, new6(A,B,D).
new7(A,B,C) :- D=0, B>=1, new6(A,B,D).
new6(A,B,C) :- new12(C,A,B,C).
new5(A,B,C) :- D=1, B=16, new6(A,B,D).
new5(A,B,C) :- B=<15, new7(A,B,C).
new5(A,B,C) :- B>=17, new7(A,B,C).
new4(A,B,C) :- D=1+A, E=2+B, A=<3, new3(D,E,C).
new4(A,B,C) :- D=1+A, A>=4, new3(D,B,C).
new3(A,B,C) :- A=<8, new4(A,B,C).
new3(A,B,C) :- A>=9, new5(A,B,C).
new2 :- A=1, B=0, new3(A,B,C).
new1 :- new2.
false :- new1.
