new5(A,B) :- A=0.
new4(A) :- B=1, A>=5, new5(B,A).
new4(A) :- B=0, A=<4, new5(B,A).
new3(A) :- A>=11, new4(A).
new2 :- new3(A).
new1 :- new2.
false :- new1.
