new13(A,B,C,D,E,F,G,H,I,J) :- A=0, new4(A,B,C,D,E,F,G,H,I,J).
new11(A,B,C,D,E,F,G,H,I,J,K) :- C=0.
new11(A,B,C,D,E,F,G,H,I,J,K) :- C=< -1, new13(A,B,D,E,F,G,H,I,J,K).
new11(A,B,C,D,E,F,G,H,I,J,K) :- C>=1, new13(A,B,D,E,F,G,H,I,J,K).
new9(A,B,C,D,E,F,G,H,I,J) :- K=1, B-J>=0, new11(A,B,K,C,D,E,F,G,H,I,J).
new9(A,B,C,D,E,F,G,H,I,J) :- K=0, B-J=< -1, new11(A,B,K,C,D,E,F,G,H,I,J).
new7(A,B,C,D,E,F,G,H,I,J,K) :- C=0.
new7(A,B,C,D,E,F,G,H,I,J,K) :- C=< -1, new9(A,B,D,E,F,G,H,I,J,K).
new7(A,B,C,D,E,F,G,H,I,J,K) :- C>=1, new9(A,B,D,E,F,G,H,I,J,K).
new6(A,B,C,D,E,F,G,H,I,J) :- K=1, J>=0, new7(A,B,K,C,D,E,F,G,H,I,J).
new6(A,B,C,D,E,F,G,H,I,J) :- K=0, J=< -1, new7(A,B,K,C,D,E,F,G,H,I,J).
new5(A,B,C,D,E,F,G,H,I,J) :- K=1+J, G-H+J=< -1, new6(A,B,C,D,E,F,G,H,I,K).
new4(A,B,C,D,E,F,G,H,I,J) :- new5(A,B,C,D,E,F,G,H,I,J).
new3(A,B,C,D,E,F,G,H,I,J) :- K=0, L=0, M=0, N=0, O=0, P=0, B>=1, 
          new4(A,B,L,M,B,N,O,B,P,K).
new2(A,B) :- new3(A,B,C,D,E,F,G,H,I,J).
new1 :- new2(A,B).
false :- new1.
