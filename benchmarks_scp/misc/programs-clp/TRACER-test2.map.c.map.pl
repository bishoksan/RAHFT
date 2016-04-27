new19(A,B,C,D,E) :- A=0.
new16(A,B,C,D,E,F,G,H) :- I=5+B+C, J=B+C, A-B>=0, new10(I,F,J,H).
new16(A,B,C,D,E,F,G,H) :- I=5+C, A-B=< -1, new10(I,F,C,H).
new14(A,B,C,D) :- E=1, F=5, G=1, H=0, new16(F,G,H,E,A,B,C,D).
new13(A,B,C,D,E,F,G,H) :- I= -1+B+C, J=B+C, A-B>=0, new14(I,J,G,H).
new13(A,B,C,D,E,F,G,H) :- I= -1+C, A-B=< -1, new14(I,C,G,H).
new12(A,B,C,D) :- E=1, F=1, G=0, new13(A,F,G,E,A,B,C,D).
new10(A,B,C,D) :- E=1, A>=1, new19(E,A,B,C,D).
new10(A,B,C,D) :- E=0, A=<0, new19(E,A,B,C,D).
new9(A,B,C,D) :- E=1+A, D=< -1, new10(E,B,C,D).
new9(A,B,C,D) :- E=1+A, D>=1, new10(E,B,C,D).
new9(A,B,C,D) :- D=0, new12(A,B,C,D).
new7(A,B,C,D) :- new9(A,B,C,E).
new6(A,B,C,D,E) :- new6(A,B,C,D,E).
new4(A,B,C,D,E) :- A=0, new6(A,B,C,D,E).
new4(A,B,C,D,E) :- A=< -1, new7(B,C,D,E).
new4(A,B,C,D,E) :- A>=1, new7(B,C,D,E).
new3(A,B,C,D) :- E=1, A>=1, new4(E,A,B,C,D).
new3(A,B,C,D) :- E=0, A=<0, new4(E,A,B,C,D).
new2 :- new3(A,B,C,D).
new1 :- new2.
false :- new1.
