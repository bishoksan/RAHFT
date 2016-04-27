new6(A,B) :- A=0.
new5(A) :- B=1, A=50, new6(B,A).
new5(A) :- B=0, A=<49, new6(B,A).
new5(A) :- B=0, A>=51, new6(B,A).
new4(A) :- A=50, new5(A).
new4(A) :- A=<49, new3(A).
new4(A) :- A>=51, new3(A).
new3(A) :- B=1+A, A=<99, new4(B).
new3(A) :- A>=100, new5(A).
new2 :- A=0, new3(A).
new1 :- new2.
false :- new1.
