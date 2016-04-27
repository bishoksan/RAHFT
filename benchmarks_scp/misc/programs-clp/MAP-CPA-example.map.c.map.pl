new6(A,B) :- B-A>=1.
new6(A,B) :- B-A=< -1.
new6(A,A) :- new3(A,A).
new5(A,B) :- B=<19.
new5(A,B) :- B>=21.
new4(A,B) :- A=20, new5(A,B).
new4(A,B) :- C=1+B, D=1+A, A=<19, new6(D,C).
new4(A,B) :- C=1+B, D=1+A, A>=21, new6(D,C).
new3(A,B) :- new4(A,B).
new2 :- A=0, B=0, new3(B,A).
new1 :- new2.
false :- new1.
