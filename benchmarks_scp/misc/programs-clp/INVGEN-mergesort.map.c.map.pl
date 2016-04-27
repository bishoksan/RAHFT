new21(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1+B, N=1+E, A=< -1, 
          new7(A,M,C,D,N,F,G,H,I,J,K,L).
new21(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1+B, N=1+E, A>=1, 
          new7(A,M,C,D,N,F,G,H,I,J,K,L).
new21(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1+E, N=1+I, A=0, 
          new7(A,B,C,D,M,F,G,H,N,J,K,L).
new18(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1+E, E-H=< -1, 
          new18(A,B,C,D,M,F,G,H,I,J,K,L).
new18(A,B,C,D,E,F,G,H,I,J,K,L) :- M=2*J+L, E-H>=0, 
          new4(A,B,C,D,E,F,G,H,I,J,K,M).
new16(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1+E, N=1+I, H-I>=1, 
          new16(A,B,C,D,M,F,G,H,N,J,K,L).
new16(A,B,C,D,E,F,G,H,I,J,K,L) :- H-I=<0, new18(A,B,C,D,F,F,G,H,I,J,K,L).
new13(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1+B, N=1+E, B-G=< -1, 
          new13(A,M,C,D,N,F,G,H,I,J,K,L).
new13(A,B,C,D,E,F,G,H,I,J,K,L) :- B-G>=0, new16(A,B,C,D,E,F,G,H,I,J,K,L).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M) :- M=0.
new11(A,B,C,D,E,F,G,H,I,J,K,L,M) :- M=< -1, new13(A,B,C,D,E,F,G,H,I,J,K,L).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M) :- M>=1, new13(A,B,C,D,E,F,G,H,I,J,K,L).
new10(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1, C-E>=0, new11(A,B,C,D,E,F,G,H,I,J,K,L,M).
new10(A,B,C,D,E,F,G,H,I,J,K,L) :- M=0, C-E=< -1, 
          new11(A,B,C,D,E,F,G,H,I,J,K,L,M).
new9(A,B,C,D,E,F,G,H,I,J,K,L) :- H-I>=1, new21(A,B,C,D,E,F,G,H,I,J,K,L).
new9(A,B,C,D,E,F,G,H,I,J,K,L) :- H-I=<0, new10(A,B,C,D,E,F,G,H,I,J,K,L).
new7(A,B,C,D,E,F,G,H,I,J,K,L) :- B-G=< -1, new9(A,B,C,D,E,F,G,H,I,J,K,L).
new7(A,B,C,D,E,F,G,H,I,J,K,L) :- B-G>=0, new10(A,B,C,D,E,F,G,H,I,J,K,L).
new5(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1+C, N=J+L, O=J+L, P=1+C, C-K=< -1, 
          new7(A,L,C,D,L,L,O,M,N,J,P,L).
new5(A,B,C,D,E,F,G,H,I,J,K,L) :- M=J+L, N=J+L, C-K>=0, 
          new7(A,L,C,D,L,L,N,K,M,J,K,L).
new4(A,B,C,D,E,F,G,H,I,J,K,L) :- M=2*J+L, C-J-L>=0, 
          new5(A,B,C,D,E,F,G,H,I,J,M,L).
new4(A,B,C,D,E,F,G,H,I,J,K,L) :- M=2*J, C-J-L=< -1, 
          new3(A,B,C,D,E,F,G,H,I,M,K,L).
new3(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1, C-J>=1, new4(A,B,C,D,E,F,G,H,I,J,K,M).
new2(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1, new3(A,B,C,D,E,F,G,H,I,M,K,L).
new1 :- new2(A,B,C,D,E,F,G,H,I,J,K,L).
false :- new1.
