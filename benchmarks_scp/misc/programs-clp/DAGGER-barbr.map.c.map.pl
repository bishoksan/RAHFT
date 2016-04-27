new134(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= -1+A, V=1+B, W=1, A>=1, 
          new3(U,V,C,W,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new133(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- D=<0, 
          new134(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new131(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= -1+A, V=1+B, W=1, A>=1, 
          new3(U,V,C,D,W,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new130(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- E=<0, 
          new131(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new128(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= -1+C, V=0, C>=1, 
          new3(A,B,U,D,V,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new127(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- E=<1, 
          new128(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new125(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= -1+A, V=1+B, W=1, A>=1, 
          new3(U,V,C,D,E,W,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new124(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- F=<0, 
          new125(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new122(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= -1+C, V=0, C>=1, 
          new3(A,B,U,D,E,V,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new121(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- F=<1, 
          new122(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new119(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= -1+A, V=1+B, W=1+G, A>=1, 
          new3(U,V,C,D,E,F,W,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new118(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- G=<0, 
          new119(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new116(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= -1+C, V= -1+G, C>=1, 
          new3(A,B,U,D,E,F,V,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new115(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- G=<1, 
          new116(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new113(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=1+A, V=1, H=<0, 
          new3(U,B,C,D,E,F,G,V,I,J,K,L,M,N,O,P,Q,R,S,T).
new111(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= -1+B, V=2, B>=1, 
          new3(A,U,C,D,E,F,G,V,I,J,K,L,M,N,O,P,Q,R,S,T).
new110(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- H=<1, 
          new111(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new108(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=1+C, V=3, H=<2, 
          new3(A,B,U,D,E,F,G,V,I,J,K,L,M,N,O,P,Q,R,S,T).
new106(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=0, C=0, 
          new3(A,B,C,D,E,F,G,U,I,J,K,L,M,N,O,P,Q,R,S,T).
new105(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- H=<3, 
          new106(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new103(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= -1+C, V=0, C>=1, 
          new3(A,B,U,V,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new102(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- D=<1, 
          new103(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new101(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- D>=1, 
          new102(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new99(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- H>=3, 
          new105(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new98(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- I=< -1, 
          new99(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new98(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- I>=1, 
          new99(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new98(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- I=0, 
          new101(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new97(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new98(A,B,C,D,E,F,G,H,U,J,K,L,M,N,O,P,Q,R,S,T).
new95(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- H>=2, 
          new108(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new94(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- J=< -1, 
          new95(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new94(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- J>=1, 
          new95(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new94(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- J=0, 
          new97(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new93(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new94(A,B,C,D,E,F,G,H,I,U,K,L,M,N,O,P,Q,R,S,T).
new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- H>=1, 
          new110(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new90(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- K=< -1, 
          new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new90(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- K>=1, 
          new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new90(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- K=0, 
          new93(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new89(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new90(A,B,C,D,E,F,G,H,I,J,U,L,M,N,O,P,Q,R,S,T).
new87(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- H>=0, 
          new113(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new86(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- L=< -1, 
          new87(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new86(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- L>=1, 
          new87(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new86(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- L=0, 
          new89(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new85(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new86(A,B,C,D,E,F,G,H,I,J,K,U,M,N,O,P,Q,R,S,T).
new83(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- G>=1, 
          new115(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new82(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- M=< -1, 
          new83(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new82(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- M>=1, 
          new83(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new82(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- M=0, 
          new85(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new81(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new82(A,B,C,D,E,F,G,H,I,J,K,L,U,N,O,P,Q,R,S,T).
new79(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- G>=0, 
          new118(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- N=< -1, 
          new79(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- N>=1, 
          new79(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- N=0, 
          new81(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new77(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new78(A,B,C,D,E,F,G,H,I,J,K,L,M,U,O,P,Q,R,S,T).
new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- F>=1, 
          new121(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new74(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- O=< -1, 
          new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new74(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- O>=1, 
          new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new74(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- O=0, 
          new77(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new73(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new74(A,B,C,D,E,F,G,H,I,J,K,L,M,N,U,P,Q,R,S,T).
new71(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- F>=0, 
          new124(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new70(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- P=< -1, 
          new71(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new70(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- P>=1, 
          new71(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new70(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- P=0, 
          new73(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new69(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new70(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,U,Q,R,S,T).
new67(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- E>=1, 
          new127(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new66(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- Q=< -1, 
          new67(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new66(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- Q>=1, 
          new67(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new66(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- Q=0, 
          new69(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new65(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new66(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,U,R,S,T).
new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- E>=0, 
          new130(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- R=< -1, 
          new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- R>=1, 
          new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- R=0, 
          new65(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new61(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,U,S,T).
new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- D>=0, 
          new133(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new58(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- S=< -1, 
          new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new58(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- S>=1, 
          new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new58(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- S=0, 
          new61(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new56(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A=0.
new54(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=1, A>=0, 
          new56(U,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new54(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=0, A=< -1, 
          new56(U,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new52(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A=0.
new52(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A=< -1, 
          new54(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new52(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A>=1, 
          new54(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new50(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=1, B>=0, 
          new52(U,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new50(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=0, B=< -1, 
          new52(U,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A=0.
new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A=< -1, 
          new50(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A>=1, 
          new50(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=1, C>=0, 
          new48(U,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=0, C=< -1, 
          new48(U,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A=0.
new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A=< -1, 
          new46(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A>=1, 
          new46(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=1, D>=0, 
          new44(U,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=0, D=< -1, 
          new44(U,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A=0.
new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A=< -1, 
          new42(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A>=1, 
          new42(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=1, E>=0, 
          new40(U,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=0, E=< -1, 
          new40(U,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A=0.
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A=< -1, 
          new38(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A>=1, 
          new38(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=1, F>=0, 
          new36(U,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=0, F=< -1, 
          new36(U,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A=0.
new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A=< -1, 
          new34(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A>=1, 
          new34(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=1, G>=0, 
          new32(U,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=0, G=< -1, 
          new32(U,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A=0.
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A=< -1, 
          new30(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A>=1, 
          new30(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=1, H=<3, 
          new28(U,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=0, H>=4, 
          new28(U,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A=0.
new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A=< -1, 
          new26(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A>=1, 
          new26(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=1, G=<1, 
          new24(U,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=0, G>=2, 
          new24(U,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A=0.
new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A=< -1, 
          new22(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A>=1, 
          new22(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=1, F=<1, 
          new20(U,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=0, F>=2, 
          new20(U,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A=0.
new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A=< -1, 
          new18(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A>=1, 
          new18(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=1, E=<1, 
          new16(U,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=0, E>=2, 
          new16(U,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A=0.
new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A=< -1, 
          new14(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A>=1, 
          new14(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=1, D=<1, 
          new12(U,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=0, D>=2, 
          new12(U,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A=0.
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A=< -1, 
          new10(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- A>=1, 
          new10(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=1, C-H=<0, 
          new8(U,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=0, C-H>=1, 
          new8(U,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new58(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,U,T).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- T=< -1, 
          new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- T>=1, 
          new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- T=0, 
          new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,U).
new2 :- A=0, B=0, C=0, D=0, E=0, F=0, G=0, H=0, 
          new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new1 :- new2.
false :- new1.
