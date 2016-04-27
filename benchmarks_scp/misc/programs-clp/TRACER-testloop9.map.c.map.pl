new16(A,B,C) :- D=1+A, E=1, F=6, B=4, new3(D,E,F).
new16(A,B,C) :- D=1+A, E=2, B=<3, new3(D,E,C).
new16(A,B,C) :- D=1+A, E=2, B>=5, new3(D,E,C).
new13(A,B,C) :- D=1+A, E=1, F=5, B=3, new3(D,E,F).
new13(A,B,C) :- B=<2, new16(A,B,C).
new13(A,B,C) :- B>=4, new16(A,B,C).
new10(A,B,C) :- D=1+A, E=3, F=4, B=2, new3(D,E,F).
new10(A,B,C) :- B=<1, new13(A,B,C).
new10(A,B,C) :- B>=3, new13(A,B,C).
new6(A,B,C,D) :- A=0.
new5(A,B,C) :- D=1, C=<5, new6(D,A,B,C).
new5(A,B,C) :- D=1, C>=7, new6(D,A,B,C).
new5(A,B,C) :- D=0, C=6, new6(D,A,B,C).
new4(A,B,C) :- D=1+A, E=2, F=3, B=1, new3(D,E,F).
new4(A,B,C) :- B=<0, new10(A,B,C).
new4(A,B,C) :- B>=2, new10(A,B,C).
new3(A,B,C) :- A=<9, new4(A,B,C).
new3(A,B,C) :- A>=10, new5(A,B,C).
new2 :- A=0, B=1, new3(A,B,C).
new1 :- new2.
false :- new1.
