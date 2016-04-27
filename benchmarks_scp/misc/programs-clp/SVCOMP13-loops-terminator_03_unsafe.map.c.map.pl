new14(A,B,C,D,E,F) :- A=0.
new10(A,B,C,D,E) :- F=1, A>=100, new8(A,B,C,D,F).
new10(A,B,C,D,E) :- F=0, A=<99, new8(A,B,C,D,F).
new9(A,B,C,D,E) :- C=< -1, new10(A,B,C,D,E).
new9(A,B,C,D,E) :- F=0, C>=0, new8(A,B,C,D,F).
new8(A,B,C,D,E) :- new14(E,A,B,C,D,E).
new7(A,B,C,D,E) :- F=1, C=<0, new8(A,B,C,D,F).
new7(A,B,C,D,E) :- C>=1, new9(A,B,C,D,E).
new6(A,B,C,D,E) :- F=A+C, A=<99, new6(F,B,C,D,E).
new6(A,B,C,D,E) :- A>=100, new7(A,B,C,D,E).
new5(A,B,C,D,E) :- C>=1, new6(A,B,C,D,E).
new5(A,B,C,D,E) :- C=<0, new7(A,B,C,D,E).
new4(A,B,C,D,E) :- new5(A,B,F,F,E).
new3(A,B,C,D,E) :- new4(F,F,C,D,E).
new2 :- new3(A,B,C,D,E).
new1 :- new2.
false :- new1.
