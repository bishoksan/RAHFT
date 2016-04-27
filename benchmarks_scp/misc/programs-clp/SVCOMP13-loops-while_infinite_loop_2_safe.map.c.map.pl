new5(A,B) :- A=0.
new5(A,B) :- A=< -1, new3(B).
new5(A,B) :- A>=1, new3(B).
new4(A) :- B=1, A=0, new5(B,A).
new4(A) :- B=0, A=< -1, new5(B,A).
new4(A) :- B=0, A>=1, new5(B,A).
new3(A) :- new4(A).
new2 :- A=0, new3(A).
new1 :- new2.
false :- new1.
