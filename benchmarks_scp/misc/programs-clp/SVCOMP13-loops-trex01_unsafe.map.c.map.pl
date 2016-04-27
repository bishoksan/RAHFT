new18(A,B,C,D,E,F,G,H,I,J) :- K= -1+E, new13(A,B,L,D,K,F,G,L,I,J).
new17(A,B,C,D,E,F,G,H,I,J) :- K= -(A)+B, F=< -1, new18(A,K,C,D,E,F,G,H,I,J).
new17(A,B,C,D,E,F,G,H,I,J) :- K= -(A)+B, F>=1, new18(A,K,C,D,E,F,G,H,I,J).
new17(A,B,C,D,E,F,G,H,I,J) :- K= -(A)+C, F=0, new13(A,B,K,D,E,F,G,H,I,J).
new16(A,B,C,D,E,F,G,H,I,J) :- new17(A,B,C,D,E,K,K,H,I,J).
new15(A,B,C,D,E,F,G,H,I,J) :- C>=1, new16(A,B,C,D,E,F,G,H,I,J).
new13(A,B,C,D,E,F,G,H,I,J) :- B>=1, new15(A,B,C,D,E,F,G,H,I,J).
new11(A,B,C,D,E,F,G,H,I,J,K) :- A=0.
new11(A,B,C,D,E,F,G,H,I,J,K) :- A=< -1, new13(B,C,D,E,F,G,H,I,J,K).
new11(A,B,C,D,E,F,G,H,I,J,K) :- A>=1, new13(B,C,D,E,F,G,H,I,J,K).
new10(A,B,C,D,E,F,G,H,I,J) :- K=1, E>=2, new11(K,A,B,C,D,E,F,G,H,I,J).
new10(A,B,C,D,E,F,G,H,I,J) :- K=0, E=<1, new11(K,A,B,C,D,E,F,G,H,I,J).
new8(A,B,C,D,E,F,G,H,I,J) :- K=2*E, D-E>=1, new8(A,B,C,D,K,F,G,H,I,J).
new8(A,B,C,D,E,F,G,H,I,J) :- D-E=<0, new10(A,B,C,D,E,F,G,H,I,J).
new7(A,B) :- C=2, D=1, new8(C,E,F,G,D,H,I,J,A,B).
new5(A,B) :- C=1, D=1, new8(C,E,F,G,D,H,I,J,A,B).
new4(A,B) :- A=< -1, new5(A,B).
new4(A,B) :- A>=1, new5(A,B).
new4(A,B) :- A=0, new7(A,B).
new3(A,B) :- new4(C,C).
new2 :- new3(A,B).
new1 :- new2.
false :- new1.
