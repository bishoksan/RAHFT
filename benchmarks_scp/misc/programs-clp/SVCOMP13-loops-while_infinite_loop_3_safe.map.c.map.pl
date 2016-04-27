new7(A,B) :- B=0.
new7(A,B) :- B=< -1, new3(A).
new7(A,B) :- B>=1, new3(A).
new6(A) :- B=1, A=0, new7(A,B).
new6(A) :- B=0, A=< -1, new7(A,B).
new6(A) :- B=0, A>=1, new7(A,B).
new5(A) :- B=0, new6(B).
new4(A) :- new5(A).
new3(A) :- new4(A).
new2(A) :- new3(A).
new1 :- A=0, new2(A).
false :- new1.
