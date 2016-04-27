new6(A,B) :- A=0.
new5(A) :- B=1, A=1, new6(B,A).
new5(A) :- B=0, A=<0, new6(B,A).
new5(A) :- B=0, A>=2, new6(B,A).
new4(A,B) :- C=1, new5(C).
new3(A) :- new4(B,A).
new2 :- new3(A).
new1 :- new2.
false :- new1.
