new26(A,B,C,D,E,F,G,H,I) :- J=1+A, G=< -1, new15(J,B,C,D,E,F,G,H,I).
new26(A,B,C,D,E,F,G,H,I) :- J=1+A, G>=1, new15(J,B,C,D,E,F,G,H,I).
new26(A,B,C,D,E,F,G,H,I) :- J= -1+A, K= -1+E, G=0, new15(J,B,C,D,K,F,G,H,I).
new24(A,B,C,D,E,F,G,H,I) :- new26(A,B,C,D,E,F,J,J,I).
new23(A,B,C,D,E,F,G,H,I,J) :- A=0.
new20(A,B,C,D,E,F,G,H,I) :- J=1, E=<100, new19(A,B,C,D,E,F,G,H,J).
new20(A,B,C,D,E,F,G,H,I) :- J=0, E>=101, new19(A,B,C,D,E,F,G,H,J).
new19(A,B,C,D,E,F,G,H,I) :- new23(I,A,B,C,D,E,F,G,H,I).
new18(A,B,C,D,E,F,G,H,I) :- J=1, A>=100, new19(A,B,C,D,E,F,G,H,J).
new18(A,B,C,D,E,F,G,H,I) :- A=<99, new20(A,B,C,D,E,F,G,H,I).
new17(A,B,C,D,E,F,G,H,I) :- E>=101, new24(A,B,C,D,E,F,G,H,I).
new17(A,B,C,D,E,F,G,H,I) :- E=<100, new18(A,B,C,D,E,F,G,H,I).
new15(A,B,C,D,E,F,G,H,I) :- A=<99, new17(A,B,C,D,E,F,G,H,I).
new15(A,B,C,D,E,F,G,H,I) :- A>=100, new18(A,B,C,D,E,F,G,H,I).
new14(A,B,C,D,E,F,G,H,I,J) :- new14(A,B,C,D,E,F,G,H,I,J).
new12(A,B,C,D,E,F,G,H,I,J) :- A=0, new14(A,B,C,D,E,F,G,H,I,J).
new12(A,B,C,D,E,F,G,H,I,J) :- A=< -1, new15(B,C,D,E,F,G,H,I,J).
new12(A,B,C,D,E,F,G,H,I,J) :- A>=1, new15(B,C,D,E,F,G,H,I,J).
new10(A,B,C,D,E,F,G,H,I) :- J=1, E=<99, new12(J,A,B,C,D,E,F,G,H,I).
new10(A,B,C,D,E,F,G,H,I) :- J=0, E>=100, new12(J,A,B,C,D,E,F,G,H,I).
new9(A,B,C,D,E,F,G,H,I,J) :- new9(A,B,C,D,E,F,G,H,I,J).
new7(A,B,C,D,E,F,G,H,I,J) :- A=0, new9(A,B,C,D,E,F,G,H,I,J).
new7(A,B,C,D,E,F,G,H,I,J) :- A=< -1, new10(B,C,D,E,F,G,H,I,J).
new7(A,B,C,D,E,F,G,H,I,J) :- A>=1, new10(B,C,D,E,F,G,H,I,J).
new6(A,B,C,D,E,F,G,H,I) :- J=1, A=<99, new7(J,A,B,C,D,E,F,G,H,I).
new6(A,B,C,D,E,F,G,H,I) :- J=0, A>=100, new7(J,A,B,C,D,E,F,G,H,I).
new5(A,B,C,D,E,F,G,H,I) :- new6(A,B,C,D,J,J,G,H,I).
new4(A,B,C,D,E,F,G,H,I) :- new5(A,B,J,J,E,F,G,H,I).
new3(A,B,C,D,E,F,G,H,I) :- new4(J,J,C,D,E,F,G,H,I).
new2 :- new3(A,B,C,D,E,F,G,H,I).
new1 :- new2.
false :- new1.
