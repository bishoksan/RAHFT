new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new8(A,B,C,D,E,F,G,H,I,J,K,O,M,N).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=C-H, L=< -1, C>=0, H>=0, 
          new22(A,B,O,D,E,F,G,H,I,J,K,L,M,N).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=C-H, C>=0, H>=0, L>=1, 
          new22(A,B,O,D,E,F,G,H,I,J,K,L,M,N).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=E-I, L=0, E>=0, I>=0, 
          new22(A,B,C,D,O,F,G,H,I,J,K,L,M,N).
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new28(A,B,C,D,E,F,G,H,I,O,K,L,M,N).
new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=A-G, J=< -1, A>=0, G>=0, 
          new22(O,B,C,D,E,F,G,H,I,J,K,L,M,N).
new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=A-G, A>=0, G>=0, J>=1, 
          new22(O,B,C,D,E,F,G,H,I,J,K,L,M,N).
new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- J=0, new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- E>=1, new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- E=0, new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- A=0.
new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=1, E=0, 
          new11(A,B,C,D,E,F,G,H,I,J,K,L,M,O).
new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=0, E>=1, 
          new11(A,B,C,D,E,F,G,H,I,J,K,L,M,O).
new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=1, C=0, 
          new11(A,B,C,D,E,F,G,H,I,J,K,L,M,O).
new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- C>=1, new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new17(N,A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=1, A=0, 
          new11(A,B,C,D,E,F,G,H,I,J,K,L,M,O).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- A>=1, new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- C>=1, new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- C=0, new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- A>=1, new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- A=0, new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new8(A,B,C,D,E,F,G,H,I,J,K,O,O,N).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new7(A,B,C,D,E,F,G,H,I,O,O,L,M,N).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=1, P=1, Q=1, R>=0, 
          new6(A,B,C,D,R,R,P,Q,O,J,K,L,M,N).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>=0, new5(A,B,O,O,E,F,G,H,I,J,K,L,M,N).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>=0, new4(O,O,C,D,E,F,G,H,I,J,K,L,M,N).
new2 :- new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new1 :- new2.
false :- new1.
