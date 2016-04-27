new12(A,B,C,D,E,F,G,H) :- I=1+A, G=< -1, new6(I,B,C,D,E,F,G,H).
new12(A,B,C,D,E,F,G,H) :- I=1+A, G>=1, new6(I,B,C,D,E,F,G,H).
new12(A,B,C,D,E,F,G,H) :- I= -1+A, J= -1+E, G=0, new6(I,B,C,D,J,F,G,H).
new10(A,B,C,D,E,F,G,H) :- new12(A,B,C,D,E,F,I,I).
new9(A,B,C,D,E,F,G,H,I) :- A=0.
new8(A,B,C,D,E,F,G,H) :- I=0, new9(I,A,B,C,D,E,F,G,H).
new7(A,B,C,D,E,F,G,H) :- E>=101, new10(A,B,C,D,E,F,G,H).
new7(A,B,C,D,E,F,G,H) :- E=<100, new8(A,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- A=<99, new7(A,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- A>=100, new8(A,B,C,D,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- new6(A,B,C,D,I,I,G,H).
new4(A,B,C,D,E,F,G,H) :- new5(A,B,I,I,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- new4(I,I,C,D,E,F,G,H).
new2 :- new3(A,B,C,D,E,F,G,H).
new1 :- new2.
false :- new1.
