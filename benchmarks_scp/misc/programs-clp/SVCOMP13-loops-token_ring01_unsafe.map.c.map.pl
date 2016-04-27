new261(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1).
new259(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1) :- M=5, 
          new261(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1).
new259(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1) :- D1=1+N, 
          M=<4, 
          new233(A,B,C,D,E,F,G,H,I,J,K,L,M,D1,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1).
new259(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1) :- D1=1+N, 
          M>=6, 
          new233(A,B,C,D,E,F,G,H,I,J,K,L,M,D1,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1).
new248(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1) :- 
          F1=1, G1=2, H1=0, I1=2, N=< -1, 
          new177(F1,B,G1,H1,E,F,G,H,I,I1,K,L,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1).
new248(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1) :- 
          F1=1, G1=2, H1=0, I1=2, N>=1, 
          new177(F1,B,G1,H1,E,F,G,H,I,I1,K,L,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1).
new248(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1) :- 
          F1=1, G1=2, H1=2, N=0, 
          new177(F1,B,G1,D,E,F,G,H,I,H1,K,L,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1).
new247(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1,F1) :- 
          G1=1, J=1, 
          new248(A,B,C,D,E,F,G,H,I,J,K,L,N,G1,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1,F1).
new247(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1,F1) :- 
          G1=0, J=<0, 
          new248(A,B,C,D,E,F,G,H,I,J,K,L,N,G1,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1,F1).
new247(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1,F1) :- 
          G1=0, J>=2, 
          new248(A,B,C,D,E,F,G,H,I,J,K,L,N,G1,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1,F1).
new246(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1,F1) :- 
          B=1, 
          new247(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1,F1).
new246(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1,F1) :- 
          G1=0, B=<0, 
          new248(A,B,C,D,E,F,G,H,I,J,K,L,N,G1,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1,F1).
new246(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1,F1) :- 
          G1=0, B>=2, 
          new248(A,B,C,D,E,F,G,H,I,J,K,L,N,G1,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1,F1).
new243(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1) :- 
          new246(A,B,C,D,E,F,G,H,I,J,K,L,F1,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1).
new241(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1) :- 
          F1=0, M=< -1, 
          new243(A,B,F1,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1).
new241(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1) :- 
          F1=0, M>=1, 
          new243(A,B,F1,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1).
new241(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1) :- 
          M=0, 
          new243(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1).
new240(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1,F1) :- 
          G1=1, I=1, 
          new241(A,B,C,D,E,F,G,H,I,J,K,L,G1,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1,F1).
new240(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1,F1) :- 
          G1=0, I=<0, 
          new241(A,B,C,D,E,F,G,H,I,J,K,L,G1,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1,F1).
new240(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1,F1) :- 
          G1=0, I>=2, 
          new241(A,B,C,D,E,F,G,H,I,J,K,L,G1,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1,F1).
new239(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1,F1) :- 
          A=1, 
          new240(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1,F1).
new239(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1,F1) :- 
          G1=0, A=<0, 
          new241(A,B,C,D,E,F,G,H,I,J,K,L,G1,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1,F1).
new239(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1,F1) :- 
          G1=0, A>=2, 
          new241(A,B,C,D,E,F,G,H,I,J,K,L,G1,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1,F1).
new238(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1) :- 
          new239(A,B,C,D,E,F,G,H,I,J,K,L,F1,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1).
new237(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1) :- 
          new238(A,B,C,D,E,F,G,H,I,J,K,L,D1,E1,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1).
new236(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1) :- 
          new237(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1).
new234(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1) :- D1=1, 
          new236(A,B,C,D,E,F,G,H,I,D1,E1,E1,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1).
new233(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1) :- 
          N-O=< -1, 
          new234(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1).
new233(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1) :- N-O>=0, 
          new177(A,B,C,D,E,F,G,H,I,J,K,L,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1).
new232(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1) :- M>=5, 
          new259(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1).
new232(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1) :- D1=1+N, 
          M=<4, 
          new233(A,B,C,D,E,F,G,H,I,J,K,L,M,D1,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1).
new231(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1) :- M=<5, 
          new232(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1).
new231(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1) :- D1=1+N, 
          M>=6, 
          new233(A,B,C,D,E,F,G,H,I,J,K,L,M,D1,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1).
new229(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1).
new226(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1) :- K-L=<0, 
          new229(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1).
new226(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1) :- K-L>=2, 
          new229(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1).
new226(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1) :- K=1+L, 
          new231(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1).
new224(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1) :- A=1, 
          new226(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1).
new224(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1) :- D1=0, 
          A=<0, 
          new223(A,B,C,D,E,F,G,H,I,J,K,L,M,D1,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1).
new224(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1) :- D1=0, 
          A>=2, 
          new223(A,B,C,D,E,F,G,H,I,J,K,L,M,D1,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1).
new223(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1) :- 
          new233(A,B,C,D,E,F,G,H,I,J,K,L,M,N,D1,D1,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1).
new222(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1) :- D1=0, 
          A=0, 
          new223(A,B,C,D,E,F,G,H,I,J,K,L,M,D1,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1).
new222(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1) :- A=< -1, 
          new224(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1).
new222(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1) :- A>=1, 
          new224(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1).
new219(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- 
          new222(A,B,C,D,E,F,G,H,I,J,K,L,Z,A1,B1,C1,M,N,O,P,Q,R,S,T,U,V,W,X,Y).
new218(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=1, Q=< -1, 
          new219(A,B,Z,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y).
new218(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=1, Q>=1, 
          new219(A,B,Z,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y).
new218(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Q=0, 
          new177(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y).
new206(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1=1, 
          D1=2, E1=1+Q, M-N=< -1, 
          new58(A,C1,C,D1,E,F,G,H,I,J,K,L,P,E1,R,S,T,U,V,W,X,Y,Z,A1,B1).
new206(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1=1+Q, 
          M-N>=0, new58(A,B,C,D,E,F,G,H,I,J,K,L,P,C1,R,S,T,U,V,W,X,Y,Z,A1,B1).
new204(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1) :- 
          E1=0, F1=2, G1=1+O, N=< -1, 
          new206(A,B,C,E1,E,F,G,H,F1,J,K,L,G1,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1).
new204(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1) :- 
          E1=0, F1=2, G1=1+O, N>=1, 
          new206(A,B,C,E1,E,F,G,H,F1,J,K,L,G1,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1).
new204(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1) :- 
          E1=2, F1=1+O, N=0, 
          new206(A,B,C,D,E,F,G,H,E1,J,K,L,F1,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1).
new203(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1) :- 
          F1=1, J=1, 
          new204(A,B,C,D,E,F,G,H,I,J,K,L,N,F1,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1).
new203(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1) :- 
          F1=0, J=<0, 
          new204(A,B,C,D,E,F,G,H,I,J,K,L,N,F1,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1).
new203(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1) :- 
          F1=0, J>=2, 
          new204(A,B,C,D,E,F,G,H,I,J,K,L,N,F1,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1).
new202(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1) :- 
          B=1, 
          new203(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1).
new202(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1) :- 
          F1=0, B=<0, 
          new204(A,B,C,D,E,F,G,H,I,J,K,L,N,F1,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1).
new202(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1) :- 
          F1=0, B>=2, 
          new204(A,B,C,D,E,F,G,H,I,J,K,L,N,F1,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1).
new199(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1) :- 
          new202(A,B,C,D,E,F,G,H,I,J,K,L,E1,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1).
new197(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1) :- 
          E1=0, M=< -1, 
          new199(A,B,E1,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1).
new197(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1) :- 
          E1=0, M>=1, 
          new199(A,B,E1,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1).
new197(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1) :- M=0, 
          new199(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1).
new196(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1) :- 
          F1=1, I=1, 
          new197(A,B,C,D,E,F,G,H,I,J,K,L,F1,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1).
new196(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1) :- 
          F1=0, I=<0, 
          new197(A,B,C,D,E,F,G,H,I,J,K,L,F1,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1).
new196(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1) :- 
          F1=0, I>=2, 
          new197(A,B,C,D,E,F,G,H,I,J,K,L,F1,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1).
new195(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1) :- 
          A=1, 
          new196(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1).
new195(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1) :- 
          F1=0, A=<0, 
          new197(A,B,C,D,E,F,G,H,I,J,K,L,F1,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1).
new195(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1) :- 
          F1=0, A>=2, 
          new197(A,B,C,D,E,F,G,H,I,J,K,L,F1,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1).
new194(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1) :- 
          new195(A,B,C,D,E,F,G,H,I,J,K,L,E1,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1).
new193(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- 
          new194(A,B,C,D,E,F,G,H,I,J,K,L,C1,D1,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new190(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- 
          new193(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new188(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1=1, 
          D1=1+K, B=1, 
          new190(A,B,C,D,E,F,G,H,C1,J,D1,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new188(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1=0, 
          B=<0, 
          new187(A,B,C,D,E,F,G,H,I,J,K,L,C1,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new188(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1=0, 
          B>=2, 
          new187(A,B,C,D,E,F,G,H,I,J,K,L,C1,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new187(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- 
          new206(A,B,C,D,E,F,G,H,I,J,K,L,M,C1,C1,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new186(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1=0, B=0, 
          new187(A,B,C,D,E,F,G,H,I,J,K,L,C1,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new186(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- B=< -1, 
          new188(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new186(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- B>=1, 
          new188(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new183(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- 
          new186(A,B,C,D,E,F,G,H,I,J,K,L,Z,A1,B1,M,N,O,P,Q,R,S,T,U,V,W,X,Y).
new182(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=1, R=< -1, 
          new183(A,B,C,Z,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y).
new182(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=1, R>=1, 
          new183(A,B,C,Z,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y).
new182(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=1+N, R=0, 
          new58(A,B,C,D,E,F,G,H,I,J,K,L,M,Z,O,P,Q,R,S,T,U,V,W,X,Y).
new179(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- 
          new182(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,Z,S,T,U,V,W,X,Y).
new177(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- D=0, 
          new179(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y).
new177(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=1+N, D=< -1, 
          new58(A,B,C,D,E,F,G,H,I,J,K,L,M,Z,O,P,Q,R,S,T,U,V,W,X,Y).
new177(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=1+N, D>=1, 
          new58(A,B,C,D,E,F,G,H,I,J,K,L,M,Z,O,P,Q,R,S,T,U,V,W,X,Y).
new176(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- 
          new218(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Z,R,S,T,U,V,W,X,Y).
new173(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- C=0, 
          new176(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y).
new173(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- C=< -1, 
          new177(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y).
new173(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- C>=1, 
          new177(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y).
new168(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=1, D=0, 
          new167(A,B,C,D,E,F,G,H,I,J,K,L,A1,O,P,Q,R,S,T,U,V,W,X,Y,Z).
new168(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=0, D=< -1, 
          new167(A,B,C,D,E,F,G,H,I,J,K,L,A1,O,P,Q,R,S,T,U,V,W,X,Y,Z).
new168(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=0, D>=1, 
          new167(A,B,C,D,E,F,G,H,I,J,K,L,A1,O,P,Q,R,S,T,U,V,W,X,Y,Z).
new167(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- M=< -1, 
          new173(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y).
new167(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- M>=1, 
          new173(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y).
new167(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=2, M=0, 
          new60(A,B,C,D,E,F,G,H,I,J,K,L,Z,T,U,V,W,X,Y).
new166(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=1, C=0, 
          new167(A,B,C,D,E,F,G,H,I,J,K,L,A1,O,P,Q,R,S,T,U,V,W,X,Y,Z).
new166(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- C=< -1, 
          new168(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z).
new166(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- C>=1, 
          new168(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z).
new148(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=2, J=1, 
          new111(A,B,C,D,E,F,G,H,I,T,K,L,M,N,O,P,Q,R,S).
new148(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- J=<0, 
          new111(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new148(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- J>=2, 
          new111(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new145(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=2, I=1, 
          new148(A,B,C,D,E,F,G,H,T,J,K,L,M,N,O,P,Q,R,S).
new145(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- I=<0, 
          new148(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new145(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- I>=2, 
          new148(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new142(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=2, H=1, 
          new145(A,B,C,D,E,F,G,T,I,J,K,L,M,N,O,P,Q,R,S).
new142(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- H=<0, 
          new145(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new142(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- H>=2, 
          new145(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new141(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=2, G=1, 
          new142(A,B,C,D,E,F,T,H,I,J,K,L,M,N,O,P,Q,R,S).
new141(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- G=<0, 
          new142(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new141(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- G>=2, 
          new142(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new138(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- 
          new141(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new136(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, N=< -1, 
          new138(A,B,C,V,E,F,G,H,I,J,K,L,O,P,Q,R,S,T,U).
new136(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, N>=1, 
          new138(A,B,C,V,E,F,G,H,I,J,K,L,O,P,Q,R,S,T,U).
new136(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- N=0, 
          new138(A,B,C,D,E,F,G,H,I,J,K,L,O,P,Q,R,S,T,U).
new135(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, J=1, 
          new136(A,B,C,D,E,F,G,H,I,J,K,L,N,W,P,Q,R,S,T,U,V).
new135(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, J=<0, 
          new136(A,B,C,D,E,F,G,H,I,J,K,L,N,W,P,Q,R,S,T,U,V).
new135(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, J>=2, 
          new136(A,B,C,D,E,F,G,H,I,J,K,L,N,W,P,Q,R,S,T,U,V).
new134(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- B=1, 
          new135(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new134(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, B=<0, 
          new136(A,B,C,D,E,F,G,H,I,J,K,L,N,W,P,Q,R,S,T,U,V).
new134(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, B>=2, 
          new136(A,B,C,D,E,F,G,H,I,J,K,L,N,W,P,Q,R,S,T,U,V).
new131(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- 
          new134(A,B,C,D,E,F,G,H,I,J,K,L,V,M,N,O,P,Q,R,S,T,U).
new129(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, M=< -1, 
          new131(A,B,V,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new129(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, M>=1, 
          new131(A,B,V,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new129(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- M=0, 
          new131(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new128(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, I=1, 
          new129(A,B,C,D,E,F,G,H,I,J,K,L,W,O,P,Q,R,S,T,U,V).
new128(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, I=<0, 
          new129(A,B,C,D,E,F,G,H,I,J,K,L,W,O,P,Q,R,S,T,U,V).
new128(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, I>=2, 
          new129(A,B,C,D,E,F,G,H,I,J,K,L,W,O,P,Q,R,S,T,U,V).
new127(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- A=1, 
          new128(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new127(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, A=<0, 
          new129(A,B,C,D,E,F,G,H,I,J,K,L,W,O,P,Q,R,S,T,U,V).
new127(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, A>=2, 
          new129(A,B,C,D,E,F,G,H,I,J,K,L,W,O,P,Q,R,S,T,U,V).
new126(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- 
          new127(A,B,C,D,E,F,G,H,I,J,K,L,V,M,N,O,P,Q,R,S,T,U).
new125(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- 
          new126(A,B,C,D,E,F,G,H,I,J,K,L,T,U,M,N,O,P,Q,R,S).
new121(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1+P, O=0, 
          new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,T,Q,R,S).
new116(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, D=0, 
          new115(A,B,C,D,E,F,G,H,I,J,K,L,W,O,P,Q,R,S,T,U,V).
new116(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, D=< -1, 
          new115(A,B,C,D,E,F,G,H,I,J,K,L,W,O,P,Q,R,S,T,U,V).
new116(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, D>=1, 
          new115(A,B,C,D,E,F,G,H,I,J,K,L,W,O,P,Q,R,S,T,U,V).
new115(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, M=< -1, 
          new121(A,B,C,D,E,F,G,H,I,J,K,L,O,P,V,R,S,T,U).
new115(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, M>=1, 
          new121(A,B,C,D,E,F,G,H,I,J,K,L,O,P,V,R,S,T,U).
new115(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1, M=0, 
          new121(A,B,C,D,E,F,G,H,I,J,K,L,O,P,V,R,S,T,U).
new114(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, C=0, 
          new115(A,B,C,D,E,F,G,H,I,J,K,L,W,O,P,Q,R,S,T,U,V).
new114(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- C=< -1, 
          new116(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new114(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- C>=1, 
          new116(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new113(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- 
          new114(A,B,C,D,E,F,G,H,I,J,K,L,V,M,N,O,P,Q,R,S,T,U).
new111(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- 
          new113(A,B,C,D,E,F,G,H,I,J,K,L,T,U,M,N,O,P,Q,R,S).
new110(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, 
          new125(A,B,C,D,E,F,T,H,I,J,K,L,M,N,O,P,Q,R,S).
new105(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=1, D=0, 
          new104(A,B,C,D,E,F,G,H,I,J,K,L,N,U,P,Q,R,S,T).
new105(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=0, D=< -1, 
          new104(A,B,C,D,E,F,G,H,I,J,K,L,N,U,P,Q,R,S,T).
new105(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=0, D>=1, 
          new104(A,B,C,D,E,F,G,H,I,J,K,L,N,U,P,Q,R,S,T).
new104(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=4, N=0, 
          new110(A,B,C,D,E,F,G,H,I,J,K,L,T,N,O,P,Q,R,S).
new104(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- N=< -1, 
          new111(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new104(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- N>=1, 
          new111(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new103(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=1, C=0, 
          new104(A,B,C,D,E,F,G,H,I,J,K,L,N,U,P,Q,R,S,T).
new103(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- C=< -1, 
          new105(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new103(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- C>=1, 
          new105(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new100(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- 
          new103(A,B,C,D,E,F,G,H,I,J,K,L,T,M,N,O,P,Q,R,S).
new97(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=2, J=1, 
          new100(A,B,C,D,E,F,G,H,I,T,K,L,M,N,O,P,Q,R,S).
new97(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- J=<0, 
          new100(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new97(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- J>=2, 
          new100(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new94(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=2, I=1, 
          new97(A,B,C,D,E,F,G,H,T,J,K,L,M,N,O,P,Q,R,S).
new94(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- I=<0, 
          new97(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new94(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- I>=2, 
          new97(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=2, H=1, 
          new94(A,B,C,D,E,F,G,T,I,J,K,L,M,N,O,P,Q,R,S).
new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- H=<0, 
          new94(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- H>=2, 
          new94(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new90(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=2, G=1, 
          new91(A,B,C,D,E,F,T,H,I,J,K,L,M,N,O,P,Q,R,S).
new90(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- G=<0, 
          new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new90(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- G>=2, 
          new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new87(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- 
          new90(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new85(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, N=< -1, 
          new87(A,B,C,V,E,F,G,H,I,J,K,L,O,P,Q,R,S,T,U).
new85(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, N>=1, 
          new87(A,B,C,V,E,F,G,H,I,J,K,L,O,P,Q,R,S,T,U).
new85(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- N=0, 
          new87(A,B,C,D,E,F,G,H,I,J,K,L,O,P,Q,R,S,T,U).
new84(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, J=1, 
          new85(A,B,C,D,E,F,G,H,I,J,K,L,N,W,P,Q,R,S,T,U,V).
new84(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, J=<0, 
          new85(A,B,C,D,E,F,G,H,I,J,K,L,N,W,P,Q,R,S,T,U,V).
new84(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, J>=2, 
          new85(A,B,C,D,E,F,G,H,I,J,K,L,N,W,P,Q,R,S,T,U,V).
new83(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- B=1, 
          new84(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new83(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, B=<0, 
          new85(A,B,C,D,E,F,G,H,I,J,K,L,N,W,P,Q,R,S,T,U,V).
new83(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, B>=2, 
          new85(A,B,C,D,E,F,G,H,I,J,K,L,N,W,P,Q,R,S,T,U,V).
new80(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- 
          new83(A,B,C,D,E,F,G,H,I,J,K,L,V,M,N,O,P,Q,R,S,T,U).
new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, M=< -1, 
          new80(A,B,V,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, M>=1, 
          new80(A,B,V,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- M=0, 
          new80(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new77(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, I=1, 
          new78(A,B,C,D,E,F,G,H,I,J,K,L,W,O,P,Q,R,S,T,U,V).
new77(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, I=<0, 
          new78(A,B,C,D,E,F,G,H,I,J,K,L,W,O,P,Q,R,S,T,U,V).
new77(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, I>=2, 
          new78(A,B,C,D,E,F,G,H,I,J,K,L,W,O,P,Q,R,S,T,U,V).
new76(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- A=1, 
          new77(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new76(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, A=<0, 
          new78(A,B,C,D,E,F,G,H,I,J,K,L,W,O,P,Q,R,S,T,U,V).
new76(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, A>=2, 
          new78(A,B,C,D,E,F,G,H,I,J,K,L,W,O,P,Q,R,S,T,U,V).
new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- 
          new76(A,B,C,D,E,F,G,H,I,J,K,L,V,M,N,O,P,Q,R,S,T,U).
new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- 
          new75(A,B,C,D,E,F,G,H,I,J,K,L,T,U,M,N,O,P,Q,R,S).
new69(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, J=0, 
          new72(A,B,C,D,E,F,G,H,I,T,K,L,M,N,O,P,Q,R,S).
new69(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- J=< -1, 
          new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new69(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- J>=1, 
          new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new66(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, I=0, 
          new69(A,B,C,D,E,F,G,H,T,J,K,L,M,N,O,P,Q,R,S).
new66(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- I=< -1, 
          new69(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new66(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- I>=1, 
          new69(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, H=0, 
          new66(A,B,C,D,E,F,G,T,I,J,K,L,M,N,O,P,Q,R,S).
new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- H=< -1, 
          new66(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- H>=1, 
          new66(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, G=0, 
          new63(A,B,C,D,E,F,T,H,I,J,K,L,M,N,O,P,Q,R,S).
new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- G=< -1, 
          new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- G>=1, 
          new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new61(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- 
          new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new60(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=3, 
          new61(A,B,C,D,E,F,G,H,I,J,K,L,T,N,O,P,Q,R,S).
new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- 
          new166(A,B,C,D,E,F,G,H,I,J,K,L,Z,M,N,O,P,Q,R,S,T,U,V,W,X,Y).
new58(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- N-O=< -1, 
          new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y).
new58(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=2, N-O>=0, 
          new60(A,B,C,D,E,F,G,H,I,J,K,L,Z,T,U,V,W,X,Y).
new57(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- 
          new58(A,B,C,D,E,F,G,H,I,J,K,L,M,N,Z,Z,Q,R,S,T,U,V,W,X,Y).
new56(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, 
          new57(A,B,C,D,E,F,G,H,I,J,K,L,U,T,V,W,X,Y,M,N,O,P,Q,R,S).
new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, P-Q=< -1, 
          new56(A,B,C,D,E,F,G,H,I,J,K,L,T,N,O,P,Q,R,S).
new52(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- 
          new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,T,T,S).
new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, U=2, J=1, 
          new52(A,B,C,D,E,F,G,H,I,U,K,L,M,N,O,T,Q,R,S).
new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, J=<0, 
          new52(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,T,Q,R,S).
new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, J>=2, 
          new52(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,T,Q,R,S).
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=2, I=1, 
          new49(A,B,C,D,E,F,G,H,T,J,K,L,M,N,O,P,Q,R,S).
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- I=<0, 
          new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- I>=2, 
          new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=2, H=1, 
          new46(A,B,C,D,E,F,G,T,I,J,K,L,M,N,O,P,Q,R,S).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- H=<0, 
          new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- H>=2, 
          new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=2, G=1, 
          new43(A,B,C,D,E,F,T,H,I,J,K,L,M,N,O,P,Q,R,S).
new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- G=<0, 
          new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- G>=2, 
          new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- 
          new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, N=< -1, 
          new39(A,B,C,V,E,F,G,H,I,J,K,L,O,P,Q,R,S,T,U).
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, N>=1, 
          new39(A,B,C,V,E,F,G,H,I,J,K,L,O,P,Q,R,S,T,U).
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- N=0, 
          new39(A,B,C,D,E,F,G,H,I,J,K,L,O,P,Q,R,S,T,U).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, J=1, 
          new37(A,B,C,D,E,F,G,H,I,J,K,L,N,W,P,Q,R,S,T,U,V).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, J=<0, 
          new37(A,B,C,D,E,F,G,H,I,J,K,L,N,W,P,Q,R,S,T,U,V).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, J>=2, 
          new37(A,B,C,D,E,F,G,H,I,J,K,L,N,W,P,Q,R,S,T,U,V).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- B=1, 
          new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, B=<0, 
          new37(A,B,C,D,E,F,G,H,I,J,K,L,N,W,P,Q,R,S,T,U,V).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, B>=2, 
          new37(A,B,C,D,E,F,G,H,I,J,K,L,N,W,P,Q,R,S,T,U,V).
new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- 
          new35(A,B,C,D,E,F,G,H,I,J,K,L,V,M,N,O,P,Q,R,S,T,U).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, M=< -1, 
          new32(A,B,V,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, M>=1, 
          new32(A,B,V,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- M=0, 
          new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, I=1, 
          new30(A,B,C,D,E,F,G,H,I,J,K,L,W,O,P,Q,R,S,T,U,V).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, I=<0, 
          new30(A,B,C,D,E,F,G,H,I,J,K,L,W,O,P,Q,R,S,T,U,V).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, I>=2, 
          new30(A,B,C,D,E,F,G,H,I,J,K,L,W,O,P,Q,R,S,T,U,V).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- A=1, 
          new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, A=<0, 
          new30(A,B,C,D,E,F,G,H,I,J,K,L,W,O,P,Q,R,S,T,U,V).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, A>=2, 
          new30(A,B,C,D,E,F,G,H,I,J,K,L,W,O,P,Q,R,S,T,U,V).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- 
          new28(A,B,C,D,E,F,G,H,I,J,K,L,V,M,N,O,P,Q,R,S,T,U).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- 
          new27(A,B,C,D,E,F,G,H,I,J,K,L,T,U,M,N,O,P,Q,R,S).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, J=0, 
          new24(A,B,C,D,E,F,G,H,I,T,K,L,M,N,O,P,Q,R,S).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- J=< -1, 
          new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- J>=1, 
          new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, I=0, 
          new21(A,B,C,D,E,F,G,H,T,J,K,L,M,N,O,P,Q,R,S).
new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- I=< -1, 
          new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- I>=1, 
          new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, H=0, 
          new18(A,B,C,D,E,F,G,T,I,J,K,L,M,N,O,P,Q,R,S).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- H=< -1, 
          new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- H>=1, 
          new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, G=0, 
          new15(A,B,C,D,E,F,T,H,I,J,K,L,M,N,O,P,Q,R,S).
new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- G=< -1, 
          new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- G>=1, 
          new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- 
          new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, F=1, 
          new11(A,B,C,T,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=2, F=<0, 
          new11(A,B,C,T,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=2, F>=2, 
          new11(A,B,C,T,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, E=1, 
          new8(A,B,T,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=2, E=<0, 
          new8(A,B,T,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=2, E>=2, 
          new8(A,B,T,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- 
          new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- 
          new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=0, 
          new5(A,B,C,D,E,F,G,H,I,J,K,L,N,O,P,Q,R,S,M).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1, O=1, new4(A,B,C,D,N,O,G,H,I,J,K,L,M).
new2(A,B,C,D,E,F,G,H,I,J,K,L) :- new3(A,B,C,D,E,F,G,H,I,J,K,L,M).
new1 :- A=2, B=2, C=2, D=2, E=0, F=0, new2(F,E,G,H,I,J,D,C,B,A,K,L).
false :- new1.
