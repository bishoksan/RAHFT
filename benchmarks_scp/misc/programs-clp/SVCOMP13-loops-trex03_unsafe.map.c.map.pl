new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- new8(A,B,C,D,E,F,G,H,I,J,K,Q,M,N,Q,P).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=C-H, L=< -1, C>=0, H>=0, 
          new22(A,B,Q,D,E,F,G,H,I,J,K,L,M,N,O,P).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=C-H, C>=0, H>=0, L>=1, 
          new22(A,B,Q,D,E,F,G,H,I,J,K,L,M,N,O,P).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=E-I, L=0, E>=0, I>=0, 
          new22(A,B,C,D,Q,F,G,H,I,J,K,L,M,N,O,P).
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- 
          new28(A,B,C,D,E,F,G,H,I,Q,K,L,M,Q,O,P).
new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=A-G, J=< -1, A>=0, G>=0, 
          new22(Q,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=A-G, A>=0, G>=0, J>=1, 
          new22(Q,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- J=0, 
          new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- E>=1, 
          new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- E=0, 
          new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=1, E=0, 
          new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q).
new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=0, E>=1, 
          new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q).
new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- A=0.
new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- 
          new13(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- C=0, 
          new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=0, C>=1, 
          new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=0, 
          new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=0, A>=1, 
          new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q).
new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- C>=1, 
          new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- C=0, 
          new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A>=1, 
          new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=0, 
          new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- new8(A,B,C,D,E,F,G,H,I,J,K,Q,Q,N,O,P).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- new7(A,B,C,D,E,F,G,H,I,Q,Q,L,M,N,O,P).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=1, R=1, S=1, T>=0, 
          new6(A,B,C,D,T,T,R,S,Q,J,K,L,M,N,O,P).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q>=0, 
          new5(A,B,Q,Q,E,F,G,H,I,J,K,L,M,N,O,P).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q>=0, 
          new4(Q,Q,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new2 :- new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new1 :- new2.
false :- new1.
