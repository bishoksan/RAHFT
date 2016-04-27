new13(A,B) :- A=0.
new8(A) :- B=4, A=3, new4(B).
new8(A) :- B=0, A=<2, new4(B).
new8(A) :- B=0, A>=4, new4(B).
new5(A) :- B=3, A=2, new4(B).
new5(A) :- A=<1, new8(A).
new5(A) :- A>=3, new8(A).
new4(A) :- B=1, A=<4, new13(B,A).
new4(A) :- B=0, A>=5, new13(B,A).
new3(A) :- B=2, A=1, new4(B).
new3(A) :- A=<0, new5(A).
new3(A) :- A>=2, new5(A).
new2 :- new3(A).
new1 :- new2.
false :- new1.
