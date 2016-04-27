new9(A,B,C,D,E,F) :- A=0.
new9(A,B,C,D,E,F) :- G=1+E, A=< -1, new7(B,C,D,G,F).
new9(A,B,C,D,E,F) :- G=1+E, A>=1, new7(B,C,D,G,F).
new8(A,B,C,D,E) :- F=1, A+D-2*E=< -1, new9(F,A,B,C,D,E).
new8(A,B,C,D,E) :- F=0, A+D-2*E>=0, new9(F,A,B,C,D,E).
new7(A,B,C,D,E) :- C-D>=1, new8(A,B,C,D,E).
new6(A,B,C,D,E) :- F=0, B+C-2*E=<0, new7(A,B,C,F,E).
new5(A,B,C,D,E) :- A-B=<0, new6(A,B,C,D,E).
new4(A,B,C,D,E) :- B-2*E=<0, new5(A,B,C,D,E).
new3(A,B,C,D,E) :- E>=1, new4(A,B,C,D,E).
new2 :- new3(A,B,C,D,E).
new1 :- new2.
false :- new1.
