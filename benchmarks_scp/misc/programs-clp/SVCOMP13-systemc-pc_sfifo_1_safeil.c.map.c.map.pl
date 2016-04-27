new118(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2, W=1, B=0, 
          new38(A,B,C,D,E,F,V,W,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new118(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- B=< -1, 
          new93(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new118(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- B>=1, 
          new93(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new107(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W) :- X=2, Y=0, Q=< -1, 
          new90(A,B,C,X,E,F,G,H,I,J,K,Y,M,N,O,R,S,T,U,V,W).
new107(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W) :- X=2, Y=0, Q>=1, 
          new90(A,B,C,X,E,F,G,H,I,J,K,Y,M,N,O,R,S,T,U,V,W).
new107(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W) :- X=2, Q=0, 
          new90(A,B,C,X,E,F,G,H,I,J,K,L,M,N,O,R,S,T,U,V,W).
new106(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y=1, D=1, 
          new107(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,Y,S,T,U,V,W,X).
new106(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y=0, D=<0, 
          new107(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,Y,S,T,U,V,W,X).
new106(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y=0, D>=2, 
          new107(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,Y,S,T,U,V,W,X).
new105(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- M=1, 
          new106(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new105(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y=0, M=<0, 
          new107(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,Y,S,T,U,V,W,X).
new105(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y=0, M>=2, 
          new107(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,Y,S,T,U,V,W,X).
new102(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W) :- 
          new105(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,X,P,Q,R,S,T,U,V,W).
new100(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W) :- X=0, P=< -1, 
          new102(A,B,C,D,E,F,X,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W).
new100(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W) :- X=0, P>=1, 
          new102(A,B,C,D,E,F,X,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W).
new100(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W) :- P=0, 
          new102(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W).
new99(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y=1, C=1, 
          new100(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Y,R,S,T,U,V,W,X).
new99(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y=0, C=<0, 
          new100(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Y,R,S,T,U,V,W,X).
new99(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y=0, C>=2, 
          new100(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Y,R,S,T,U,V,W,X).
new98(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- H=1, 
          new99(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new98(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y=0, H=<0, 
          new100(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Y,R,S,T,U,V,W,X).
new98(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y=0, H>=2, 
          new100(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Y,R,S,T,U,V,W,X).
new97(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W) :- 
          new98(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,X,P,Q,R,S,T,U,V,W).
new96(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- 
          new97(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,V,W,P,Q,R,S,T,U).
new93(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1, W=1+E, X=0, 
          new96(Y,X,C,V,W,Y,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- H=1, 
          new93(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- H=<0, 
          new90(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- H>=2, 
          new90(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new90(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- 
          new118(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new89(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- H=0, 
          new90(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new89(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- H=< -1, 
          new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new89(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- H>=1, 
          new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new86(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- 
          new89(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new85(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1, P=< -1, 
          new86(A,B,C,D,E,F,V,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new85(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1, P>=1, 
          new86(A,B,C,D,E,F,V,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new85(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- P=0, 
          new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new81(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=2, X=1, B=1, 
          new14(A,B,C,D,E,F,G,H,I,J,K,W,X,N,P,Q,R,S,T,U,V).
new81(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, X=1+J, Y=1, B=<0, 
          new51(A,Y,W,D,E,F,G,H,I,X,A,L,M,N,O,A,Q,R,S,T,U,V).
new81(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, X=1+J, Y=1, B>=2, 
          new51(A,Y,W,D,E,F,G,H,I,X,A,L,M,N,O,A,Q,R,S,T,U,V).
new73(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new70(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new69(A,B,C,D,E,F,G,H,I,E,J,K,L,M,N,O,P,Q,R,S,T,U) :- 
          new48(A,B,C,D,E,F,G,H,I,E,J,K,L,M,N,O,P,Q,R,S,T,U).
new69(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- E-J=< -1, 
          new73(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new69(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- E-J>=1, 
          new73(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new66(A,B,C,D,E,F,G,H,I,J,F,K,L,M,N,O,P,Q,R,S,T,U) :- 
          new69(A,B,C,D,E,F,G,H,I,J,F,K,L,M,N,O,P,Q,R,S,T,U).
new66(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- F-K=< -1, 
          new70(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new66(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- F-K>=1, 
          new70(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y=2, Z=0, Q=< -1, 
          new66(A,B,Y,D,E,F,G,H,I,J,K,Z,M,N,O,R,S,T,U,V,W,X).
new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y=2, Z=0, Q>=1, 
          new66(A,B,Y,D,E,F,G,H,I,J,K,Z,M,N,O,R,S,T,U,V,W,X).
new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y=2, Q=0, 
          new66(A,B,Y,D,E,F,G,H,I,J,K,L,M,N,O,R,S,T,U,V,W,X).
new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=1, D=1, 
          new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,Z,S,T,U,V,W,X,Y).
new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=0, D=<0, 
          new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,Z,S,T,U,V,W,X,Y).
new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=0, D>=2, 
          new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,Z,S,T,U,V,W,X,Y).
new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- M=1, 
          new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y).
new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=0, M=<0, 
          new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,Z,S,T,U,V,W,X,Y).
new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=0, M>=2, 
          new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,Z,S,T,U,V,W,X,Y).
new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- 
          new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Y,P,Q,R,S,T,U,V,W,X).
new57(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y=0, P=< -1, 
          new59(A,B,C,D,E,F,Y,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new57(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y=0, P>=1, 
          new59(A,B,C,D,E,F,Y,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new57(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- P=0, 
          new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new56(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=1, C=1, 
          new57(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Z,R,S,T,U,V,W,X,Y).
new56(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=0, C=<0, 
          new57(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Z,R,S,T,U,V,W,X,Y).
new56(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=0, C>=2, 
          new57(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Z,R,S,T,U,V,W,X,Y).
new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- H=1, 
          new56(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y).
new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=0, H=<0, 
          new57(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Z,R,S,T,U,V,W,X,Y).
new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=0, H>=2, 
          new57(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Z,R,S,T,U,V,W,X,Y).
new54(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- 
          new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Y,P,Q,R,S,T,U,V,W,X).
new51(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new54(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,W,X,P,Q,R,S,T,U,V).
new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, X=1+J, Y=1, M=1, 
          new51(A,Y,W,D,E,F,G,H,I,X,A,L,M,N,O,A,Q,R,S,T,U,V).
new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- M=<0, 
          new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- M>=2, 
          new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new81(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new47(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- M=0, 
          new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new47(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- M=< -1, 
          new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new47(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- M>=1, 
          new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- 
          new47(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,V,P,Q,R,S,T,U).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1, Q=< -1, 
          new44(A,B,C,D,E,F,G,H,I,J,K,V,M,N,O,P,Q,R,S,T,U).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1, Q>=1, 
          new44(A,B,C,D,E,F,G,H,I,J,K,V,M,N,O,P,Q,R,S,T,U).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- Q=0, 
          new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- 
          new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,V,R,S,T,U).
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- L=0, 
          new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- L=< -1, 
          new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- L>=1, 
          new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- 
          new85(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,V,Q,R,S,T,U).
new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- Q=0, 
          new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1, L=0, 
          new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,V,R,S,T,U).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, L=< -1, 
          new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,V,R,S,T,U).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, L>=1, 
          new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,V,R,S,T,U).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=0, P=< -1, 
          new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,R,U,T).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=0, P>=1, 
          new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,R,U,T).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=1, P=0, 
          new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,R,U,T).
new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1, G=0, 
          new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,V,R,S,T,U).
new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- G=< -1, 
          new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- G>=1, 
          new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,U,P,Q,R,S,T).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,S,T,P,Q,R).
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- G=0, 
          new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- G=< -1, 
          new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- G>=1, 
          new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, L=0, 
          new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,W,T,U,V).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, L=< -1, 
          new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,W,T,U,V).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, L>=1, 
          new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,W,T,U,V).
new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- R=< -1, 
          new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- R>=1, 
          new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- R=0, 
          new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,S,T,U).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, G=0, 
          new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,W,T,U,V).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- G=< -1, 
          new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- G>=1, 
          new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- 
          new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,V,P,Q,R,S,T,U).
new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,S,T,U,P,Q,R).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=1, 
          new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,S,Q,R).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=0, N=1, 
          new10(A,B,C,D,E,F,G,H,I,J,K,S,M,N,O,P,Q,R).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=2, N=<0, 
          new10(A,B,C,D,E,F,G,H,I,J,K,S,M,N,O,P,Q,R).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=2, N>=2, 
          new10(A,B,C,D,E,F,G,H,I,J,K,S,M,N,O,P,Q,R).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=0, I=1, 
          new7(A,B,C,D,E,F,S,H,I,J,K,L,M,N,O,P,Q,R).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=2, I=<0, 
          new7(A,B,C,D,E,F,S,H,I,J,K,L,M,N,O,P,Q,R).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=2, I>=2, 
          new7(A,B,C,D,E,F,S,H,I,J,K,L,M,N,O,P,Q,R).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=0, 
          new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,P).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=1, R=2, S=2, T=0, U=0, V=1, W=0, 
          X=0, Y=1, new4(A,Q,R,S,T,F,G,U,V,W,K,L,X,Y,O,P).
new2(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new1 :- new2(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
false :- new1.
