new26(A,B,C,D,E,F,G,H,I,J) :- A=0, new17(A,B,C,D,E,F,G,H,I,J).
new24(A,B,C,D,E,F,G,H,I,J,K) :- B=0.
new24(A,B,C,D,E,F,G,H,I,J,K) :- L=1+K, B=< -1, new26(A,C,D,E,F,G,H,I,J,L).
new24(A,B,C,D,E,F,G,H,I,J,K) :- L=1+K, B>=1, new26(A,C,D,E,F,G,H,I,J,L).
new22(A,B,C,D,E,F,G,H,I,J) :- K=1, E-J>=0, new24(A,K,B,C,D,E,F,G,H,I,J).
new22(A,B,C,D,E,F,G,H,I,J) :- K=0, E-J=< -1, new24(A,K,B,C,D,E,F,G,H,I,J).
new20(A,B,C,D,E,F,G,H,I,J,K) :- B=0.
new20(A,B,C,D,E,F,G,H,I,J,K) :- B=< -1, new22(A,C,D,E,F,G,H,I,J,K).
new20(A,B,C,D,E,F,G,H,I,J,K) :- B>=1, new22(A,C,D,E,F,G,H,I,J,K).
new19(A,B,C,D,E,F,G,H,I,J) :- K=1, J>=0, new20(A,K,B,C,D,E,F,G,H,I,J).
new19(A,B,C,D,E,F,G,H,I,J) :- K=0, J=< -1, new20(A,K,B,C,D,E,F,G,H,I,J).
new18(A,B,C,D,E,F,G,H,I,J) :- E-J>=0, new19(A,B,C,D,E,F,G,H,I,J).
new17(A,B,C,D,E,F,G,H,I,J) :- new18(A,B,C,D,E,F,G,H,I,J).
new15(A,B,C,D,E,F,G,H,I,J) :- new4(A,B,C,D,E,F,G,H,I,J).
new13(A,B,C,D,E,F,G,H,I,J) :- A=< -1, new15(A,B,C,D,E,F,G,H,I,J).
new13(A,B,C,D,E,F,G,H,I,J) :- A>=1, new15(A,B,C,D,E,F,G,H,I,J).
new13(A,B,C,D,E,F,G,H,I,J) :- K=0, A=0, new17(A,B,C,D,E,F,G,H,I,K).
new11(A,B,C,D,E,F,G,H,I,J,K) :- B=0.
new11(A,B,C,D,E,F,G,H,I,J,K) :- B=< -1, new13(A,C,D,E,F,G,H,I,J,K).
new11(A,B,C,D,E,F,G,H,I,J,K) :- B>=1, new13(A,C,D,E,F,G,H,I,J,K).
new9(A,B,C,D,E,F,G,H,I,J) :- K=1, C-E=<0, new11(A,K,B,C,D,E,F,G,H,I,J).
new9(A,B,C,D,E,F,G,H,I,J) :- K=0, C-E>=1, new11(A,K,B,C,D,E,F,G,H,I,J).
new7(A,B,C,D,E,F,G,H,I,J,K) :- B=0.
new7(A,B,C,D,E,F,G,H,I,J,K) :- B=< -1, new9(A,C,D,E,F,G,H,I,J,K).
new7(A,B,C,D,E,F,G,H,I,J,K) :- B>=1, new9(A,C,D,E,F,G,H,I,J,K).
new5(A,B,C,D,E,F,G,H,I,J) :- K=1, C>=0, new7(A,K,B,C,D,E,F,G,H,I,J).
new5(A,B,C,D,E,F,G,H,I,J) :- K=0, C=< -1, new7(A,K,B,C,D,E,F,G,H,I,J).
new4(A,B,C,D,E,F,G,H,I,J) :- A=< -1, new5(A,B,C,D,E,F,G,H,I,J).
new4(A,B,C,D,E,F,G,H,I,J) :- A>=1, new5(A,B,C,D,E,F,G,H,I,J).
new3(A,B,C,D,E,F,G,H,I,J) :- K=0, L=0, M=0, N=0, O=0, E>=1, 
          new4(A,K,L,E,E,M,N,O,E,J).
new2(A) :- new3(A,B,C,D,E,F,G,H,I,J).
new1 :- new2(A).
false :- new1.
