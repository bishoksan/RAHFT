new322(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=2, A1=0, Q=< -1, 
          new299(A,B,C,D,E,A1,G,H,I,J,K,L,Z,N,R,S,T,U,V,W,X,Y).
new322(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=2, A1=0, Q>=1, 
          new299(A,B,C,D,E,A1,G,H,I,J,K,L,Z,N,R,S,T,U,V,W,X,Y).
new322(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=2, Q=0, 
          new299(A,B,C,D,E,F,G,H,I,J,K,L,Z,N,R,S,T,U,V,W,X,Y).
new321(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=1, N=1, 
          new322(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,Q,A1,S,T,U,V,W,X,Y,Z).
new321(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=0, N=<0, 
          new322(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,Q,A1,S,T,U,V,W,X,Y,Z).
new321(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=0, N>=2, 
          new322(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,Q,A1,S,T,U,V,W,X,Y,Z).
new320(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- C=1, 
          new321(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z).
new320(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=0, C=<0, 
          new322(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,Q,A1,S,T,U,V,W,X,Y,Z).
new320(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=0, C>=2, 
          new322(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,Q,A1,S,T,U,V,W,X,Y,Z).
new317(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- 
          new320(A,B,C,D,E,F,G,H,I,J,K,L,M,N,Z,O,P,Q,R,S,T,U,V,W,X,Y).
new315(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=0, P=< -1, 
          new317(A,B,C,D,Z,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y).
new315(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=0, P>=1, 
          new317(A,B,C,D,Z,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y).
new315(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- P=0, 
          new317(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y).
new314(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=1, M=1, 
          new315(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,A1,R,S,T,U,V,W,X,Y,Z).
new314(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=0, M=<0, 
          new315(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,A1,R,S,T,U,V,W,X,Y,Z).
new314(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=0, M>=2, 
          new315(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,A1,R,S,T,U,V,W,X,Y,Z).
new313(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- B=1, 
          new314(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z).
new313(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=0, B=<0, 
          new315(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,A1,R,S,T,U,V,W,X,Y,Z).
new313(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=0, B>=2, 
          new315(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,A1,R,S,T,U,V,W,X,Y,Z).
new310(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- 
          new313(A,B,C,D,E,F,G,H,I,J,K,L,M,N,Z,O,P,Q,R,S,T,U,V,W,X,Y).
new308(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=0, O=< -1, 
          new310(A,B,C,Z,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y).
new308(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=0, O>=1, 
          new310(A,B,C,Z,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y).
new308(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- O=0, 
          new310(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y).
new307(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=1, J=1, 
          new308(A,B,C,D,E,F,G,H,I,J,K,L,M,N,A1,Q,R,S,T,U,V,W,X,Y,Z).
new307(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=0, J=<0, 
          new308(A,B,C,D,E,F,G,H,I,J,K,L,M,N,A1,Q,R,S,T,U,V,W,X,Y,Z).
new307(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=0, J>=2, 
          new308(A,B,C,D,E,F,G,H,I,J,K,L,M,N,A1,Q,R,S,T,U,V,W,X,Y,Z).
new306(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A=1, 
          new307(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z).
new306(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=0, A=<0, 
          new308(A,B,C,D,E,F,G,H,I,J,K,L,M,N,A1,Q,R,S,T,U,V,W,X,Y,Z).
new306(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=0, A>=2, 
          new308(A,B,C,D,E,F,G,H,I,J,K,L,M,N,A1,Q,R,S,T,U,V,W,X,Y,Z).
new305(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- 
          new306(A,B,C,D,E,F,G,H,I,J,K,L,M,N,Z,O,P,Q,R,S,T,U,V,W,X,Y).
new304(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new305(A,B,C,D,E,F,G,H,I,J,K,L,M,N,W,X,Y,O,P,Q,R,S,T,U,V).
new303(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new304(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new299(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, X=2, 
          new227(W,B,C,X,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new297(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- A=1, 
          new299(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new297(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- A=<0, 
          new296(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new297(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- A>=2, 
          new296(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new296(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, 
          new303(A,B,C,D,E,F,G,H,I,J,K,L,W,N,O,P,Q,R,S,T,U,V).
new295(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- A=0, 
          new296(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new295(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- A=< -1, 
          new297(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new295(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- A>=1, 
          new297(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new292(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new295(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new291(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, P=< -1, 
          new292(A,B,C,W,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new291(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, P>=1, 
          new292(A,B,C,W,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new291(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- P=0, 
          new227(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new276(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=2, A1=0, Q=< -1, 
          new252(A,B,C,D,E,A1,G,H,I,J,K,L,M,Z,R,S,T,U,V,W,X,Y).
new276(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=2, A1=0, Q>=1, 
          new252(A,B,C,D,E,A1,G,H,I,J,K,L,M,Z,R,S,T,U,V,W,X,Y).
new276(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=2, Q=0, 
          new252(A,B,C,D,E,F,G,H,I,J,K,L,M,Z,R,S,T,U,V,W,X,Y).
new275(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=1, N=1, 
          new276(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,Q,A1,S,T,U,V,W,X,Y,Z).
new275(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=0, N=<0, 
          new276(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,Q,A1,S,T,U,V,W,X,Y,Z).
new275(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=0, N>=2, 
          new276(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,Q,A1,S,T,U,V,W,X,Y,Z).
new274(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- C=1, 
          new275(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z).
new274(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=0, C=<0, 
          new276(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,Q,A1,S,T,U,V,W,X,Y,Z).
new274(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=0, C>=2, 
          new276(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,Q,A1,S,T,U,V,W,X,Y,Z).
new271(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- 
          new274(A,B,C,D,E,F,G,H,I,J,K,L,M,N,Z,O,P,Q,R,S,T,U,V,W,X,Y).
new269(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=0, P=< -1, 
          new271(A,B,C,D,Z,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y).
new269(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=0, P>=1, 
          new271(A,B,C,D,Z,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y).
new269(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- P=0, 
          new271(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y).
new268(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=1, M=1, 
          new269(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,A1,R,S,T,U,V,W,X,Y,Z).
new268(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=0, M=<0, 
          new269(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,A1,R,S,T,U,V,W,X,Y,Z).
new268(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=0, M>=2, 
          new269(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,A1,R,S,T,U,V,W,X,Y,Z).
new267(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- B=1, 
          new268(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z).
new267(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=0, B=<0, 
          new269(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,A1,R,S,T,U,V,W,X,Y,Z).
new267(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=0, B>=2, 
          new269(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,A1,R,S,T,U,V,W,X,Y,Z).
new264(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- 
          new267(A,B,C,D,E,F,G,H,I,J,K,L,M,N,Z,O,P,Q,R,S,T,U,V,W,X,Y).
new262(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=0, O=< -1, 
          new264(A,B,C,Z,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y).
new262(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- Z=0, O>=1, 
          new264(A,B,C,Z,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y).
new262(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- O=0, 
          new264(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y).
new261(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=1, J=1, 
          new262(A,B,C,D,E,F,G,H,I,J,K,L,M,N,A1,Q,R,S,T,U,V,W,X,Y,Z).
new261(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=0, J=<0, 
          new262(A,B,C,D,E,F,G,H,I,J,K,L,M,N,A1,Q,R,S,T,U,V,W,X,Y,Z).
new261(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=0, J>=2, 
          new262(A,B,C,D,E,F,G,H,I,J,K,L,M,N,A1,Q,R,S,T,U,V,W,X,Y,Z).
new260(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A=1, 
          new261(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z).
new260(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=0, A=<0, 
          new262(A,B,C,D,E,F,G,H,I,J,K,L,M,N,A1,Q,R,S,T,U,V,W,X,Y,Z).
new260(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :- A1=0, A>=2, 
          new262(A,B,C,D,E,F,G,H,I,J,K,L,M,N,A1,Q,R,S,T,U,V,W,X,Y,Z).
new259(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :- 
          new260(A,B,C,D,E,F,G,H,I,J,K,L,M,N,Z,O,P,Q,R,S,T,U,V,W,X,Y).
new258(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new259(A,B,C,D,E,F,G,H,I,J,K,L,M,N,W,X,Y,O,P,Q,R,S,T,U,V).
new255(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new258(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new253(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, B=1, 
          new255(A,B,C,D,E,F,G,H,I,J,K,L,M,W,O,P,Q,R,S,T,U,V).
new253(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- B=<0, 
          new252(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new253(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- B>=2, 
          new252(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new252(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, X=2, 
          new230(A,W,C,D,X,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new251(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- B=0, 
          new252(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new251(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- B=< -1, 
          new253(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new251(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- B>=1, 
          new253(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new248(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new251(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new247(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, Q=< -1, 
          new248(A,B,C,D,W,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new247(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, Q>=1, 
          new248(A,B,C,D,W,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new247(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- Q=0, 
          new230(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new243(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new241(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- C=1, 
          new243(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new241(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- C=<0, 
          new240(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new241(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- C>=2, 
          new240(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new240(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, X=2, 
          new72(A,B,W,D,E,X,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new239(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- C=0, 
          new240(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new239(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- C=< -1, 
          new241(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new239(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- C>=1, 
          new241(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new236(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new239(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new235(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, R=< -1, 
          new236(A,B,C,D,E,W,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new235(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, R>=1, 
          new236(A,B,C,D,E,W,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new235(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- R=0, 
          new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new232(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new235(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,W,S,T,U,V).
new230(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- F=0, 
          new232(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new230(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- F=< -1, 
          new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new230(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- F>=1, 
          new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new229(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new247(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,W,R,S,T,U,V).
new227(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- E=0, 
          new229(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new227(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- E=< -1, 
          new230(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new227(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- E>=1, 
          new230(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new226(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new291(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,W,Q,R,S,T,U,V).
new202(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=2, N=1, 
          new152(A,B,C,D,E,F,G,H,I,J,K,L,M,S,O,P,Q,R).
new202(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- N=<0, 
          new152(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new202(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- N>=2, 
          new152(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new199(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=2, M=1, 
          new202(A,B,C,D,E,F,G,H,I,J,K,L,S,N,O,P,Q,R).
new199(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- M=<0, 
          new202(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new199(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- M>=2, 
          new202(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new196(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=2, L=1, 
          new199(A,B,C,D,E,F,G,H,I,J,K,S,M,N,O,P,Q,R).
new196(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- L=<0, 
          new199(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new196(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- L>=2, 
          new199(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new193(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=2, K=1, 
          new196(A,B,C,D,E,F,G,H,I,J,S,L,M,N,O,P,Q,R).
new193(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- K=<0, 
          new196(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new193(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- K>=2, 
          new196(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new192(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=2, J=1, 
          new193(A,B,C,D,E,F,G,H,I,S,K,L,M,N,O,P,Q,R).
new192(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- J=<0, 
          new193(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new192(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- J>=2, 
          new193(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new189(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new192(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new187(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, Q=< -1, 
          new189(A,B,C,D,E,V,G,H,I,J,K,L,M,N,R,S,T,U).
new187(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, Q>=1, 
          new189(A,B,C,D,E,V,G,H,I,J,K,L,M,N,R,S,T,U).
new187(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- Q=0, 
          new189(A,B,C,D,E,F,G,H,I,J,K,L,M,N,R,S,T,U).
new186(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, N=1, 
          new187(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,Q,W,S,T,U,V).
new186(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, N=<0, 
          new187(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,Q,W,S,T,U,V).
new186(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, N>=2, 
          new187(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,Q,W,S,T,U,V).
new185(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- C=1, 
          new186(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new185(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, C=<0, 
          new187(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,Q,W,S,T,U,V).
new185(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, C>=2, 
          new187(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,Q,W,S,T,U,V).
new182(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- 
          new185(A,B,C,D,E,F,G,H,I,J,K,L,M,N,V,O,P,Q,R,S,T,U).
new180(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, P=< -1, 
          new182(A,B,C,D,V,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new180(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, P>=1, 
          new182(A,B,C,D,V,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new180(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- P=0, 
          new182(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new179(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, M=1, 
          new180(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,W,R,S,T,U,V).
new179(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, M=<0, 
          new180(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,W,R,S,T,U,V).
new179(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, M>=2, 
          new180(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,W,R,S,T,U,V).
new178(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- B=1, 
          new179(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new178(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, B=<0, 
          new180(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,W,R,S,T,U,V).
new178(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, B>=2, 
          new180(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,W,R,S,T,U,V).
new175(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- 
          new178(A,B,C,D,E,F,G,H,I,J,K,L,M,N,V,O,P,Q,R,S,T,U).
new173(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, O=< -1, 
          new175(A,B,C,V,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new173(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, O>=1, 
          new175(A,B,C,V,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new173(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- O=0, 
          new175(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new172(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, J=1, 
          new173(A,B,C,D,E,F,G,H,I,J,K,L,M,N,W,Q,R,S,T,U,V).
new172(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, J=<0, 
          new173(A,B,C,D,E,F,G,H,I,J,K,L,M,N,W,Q,R,S,T,U,V).
new172(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, J>=2, 
          new173(A,B,C,D,E,F,G,H,I,J,K,L,M,N,W,Q,R,S,T,U,V).
new171(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- A=1, 
          new172(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new171(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, A=<0, 
          new173(A,B,C,D,E,F,G,H,I,J,K,L,M,N,W,Q,R,S,T,U,V).
new171(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, A>=2, 
          new173(A,B,C,D,E,F,G,H,I,J,K,L,M,N,W,Q,R,S,T,U,V).
new170(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- 
          new171(A,B,C,D,E,F,G,H,I,J,K,L,M,N,V,O,P,Q,R,S,T,U).
new169(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new170(A,B,C,D,E,F,G,H,I,J,K,L,M,N,S,T,U,O,P,Q,R).
new165(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- Q=0, 
          new68(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new160(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1, F=0, 
          new156(A,B,C,D,E,F,G,H,I,J,K,L,M,N,V,Q,R,S,T,U).
new160(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, F=< -1, 
          new156(A,B,C,D,E,F,G,H,I,J,K,L,M,N,V,Q,R,S,T,U).
new160(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, F>=1, 
          new156(A,B,C,D,E,F,G,H,I,J,K,L,M,N,V,Q,R,S,T,U).
new157(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1, E=0, 
          new156(A,B,C,D,E,F,G,H,I,J,K,L,M,N,V,Q,R,S,T,U).
new157(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- E=< -1, 
          new160(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new157(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- E>=1, 
          new160(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new156(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=0, O=< -1, 
          new165(A,B,C,D,E,F,G,H,I,J,K,L,M,N,Q,R,U,T).
new156(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=0, O>=1, 
          new165(A,B,C,D,E,F,G,H,I,J,K,L,M,N,Q,R,U,T).
new156(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=1, O=0, 
          new165(A,B,C,D,E,F,G,H,I,J,K,L,M,N,Q,R,U,T).
new155(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1, D=0, 
          new156(A,B,C,D,E,F,G,H,I,J,K,L,M,N,V,Q,R,S,T,U).
new155(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- D=< -1, 
          new157(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new155(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- D>=1, 
          new157(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new154(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new155(A,B,C,D,E,F,G,H,I,J,K,L,M,N,U,O,P,Q,R,S,T).
new152(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new154(A,B,C,D,E,F,G,H,I,J,K,L,M,N,S,T,O,P,Q,R).
new151(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=1, 
          new169(A,B,C,D,E,F,G,H,I,S,K,L,M,N,O,P,Q,R).
new146(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, F=0, 
          new142(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,T,R,S).
new146(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, F=< -1, 
          new142(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,T,R,S).
new146(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, F>=1, 
          new142(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,T,R,S).
new143(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, E=0, 
          new142(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,T,R,S).
new143(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- E=< -1, 
          new146(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new143(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- E>=1, 
          new146(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new142(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=4, P=0, 
          new151(A,B,C,D,E,F,G,H,I,J,K,L,M,N,S,P,Q,R).
new142(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- P=< -1, 
          new152(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new142(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- P>=1, 
          new152(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new141(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, D=0, 
          new142(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,T,R,S).
new141(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- D=< -1, 
          new143(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new141(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- D>=1, 
          new143(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new138(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new141(A,B,C,D,E,F,G,H,I,J,K,L,M,N,S,O,P,Q,R).
new135(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=2, N=1, 
          new138(A,B,C,D,E,F,G,H,I,J,K,L,M,S,O,P,Q,R).
new135(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- N=<0, 
          new138(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new135(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- N>=2, 
          new138(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new132(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=2, M=1, 
          new135(A,B,C,D,E,F,G,H,I,J,K,L,S,N,O,P,Q,R).
new132(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- M=<0, 
          new135(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new132(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- M>=2, 
          new135(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new129(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=2, L=1, 
          new132(A,B,C,D,E,F,G,H,I,J,K,S,M,N,O,P,Q,R).
new129(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- L=<0, 
          new132(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new129(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- L>=2, 
          new132(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new126(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=2, K=1, 
          new129(A,B,C,D,E,F,G,H,I,J,S,L,M,N,O,P,Q,R).
new126(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- K=<0, 
          new129(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new126(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- K>=2, 
          new129(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new125(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=2, J=1, 
          new126(A,B,C,D,E,F,G,H,I,S,K,L,M,N,O,P,Q,R).
new125(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- J=<0, 
          new126(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new125(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- J>=2, 
          new126(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new122(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new125(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new120(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, Q=< -1, 
          new122(A,B,C,D,E,V,G,H,I,J,K,L,M,N,R,S,T,U).
new120(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, Q>=1, 
          new122(A,B,C,D,E,V,G,H,I,J,K,L,M,N,R,S,T,U).
new120(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- Q=0, 
          new122(A,B,C,D,E,F,G,H,I,J,K,L,M,N,R,S,T,U).
new119(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, N=1, 
          new120(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,Q,W,S,T,U,V).
new119(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, N=<0, 
          new120(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,Q,W,S,T,U,V).
new119(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, N>=2, 
          new120(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,Q,W,S,T,U,V).
new118(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- C=1, 
          new119(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new118(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, C=<0, 
          new120(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,Q,W,S,T,U,V).
new118(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, C>=2, 
          new120(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,Q,W,S,T,U,V).
new115(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- 
          new118(A,B,C,D,E,F,G,H,I,J,K,L,M,N,V,O,P,Q,R,S,T,U).
new113(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, P=< -1, 
          new115(A,B,C,D,V,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new113(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, P>=1, 
          new115(A,B,C,D,V,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new113(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- P=0, 
          new115(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new112(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, M=1, 
          new113(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,W,R,S,T,U,V).
new112(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, M=<0, 
          new113(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,W,R,S,T,U,V).
new112(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, M>=2, 
          new113(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,W,R,S,T,U,V).
new111(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- B=1, 
          new112(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new111(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, B=<0, 
          new113(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,W,R,S,T,U,V).
new111(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, B>=2, 
          new113(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,W,R,S,T,U,V).
new108(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- 
          new111(A,B,C,D,E,F,G,H,I,J,K,L,M,N,V,O,P,Q,R,S,T,U).
new106(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, O=< -1, 
          new108(A,B,C,V,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new106(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, O>=1, 
          new108(A,B,C,V,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new106(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- O=0, 
          new108(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new105(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, J=1, 
          new106(A,B,C,D,E,F,G,H,I,J,K,L,M,N,W,Q,R,S,T,U,V).
new105(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, J=<0, 
          new106(A,B,C,D,E,F,G,H,I,J,K,L,M,N,W,Q,R,S,T,U,V).
new105(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, J>=2, 
          new106(A,B,C,D,E,F,G,H,I,J,K,L,M,N,W,Q,R,S,T,U,V).
new104(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- A=1, 
          new105(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new104(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, A=<0, 
          new106(A,B,C,D,E,F,G,H,I,J,K,L,M,N,W,Q,R,S,T,U,V).
new104(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, A>=2, 
          new106(A,B,C,D,E,F,G,H,I,J,K,L,M,N,W,Q,R,S,T,U,V).
new103(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- 
          new104(A,B,C,D,E,F,G,H,I,J,K,L,M,N,V,O,P,Q,R,S,T,U).
new100(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new103(A,B,C,D,E,F,G,H,I,J,K,L,M,N,S,T,U,O,P,Q,R).
new97(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=1, N=0, 
          new100(A,B,C,D,E,F,G,H,I,J,K,L,M,S,O,P,Q,R).
new97(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- N=< -1, 
          new100(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new97(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- N>=1, 
          new100(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new94(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=1, M=0, 
          new97(A,B,C,D,E,F,G,H,I,J,K,L,S,N,O,P,Q,R).
new94(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- M=< -1, 
          new97(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new94(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- M>=1, 
          new97(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=1, L=0, 
          new94(A,B,C,D,E,F,G,H,I,J,K,S,M,N,O,P,Q,R).
new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- L=< -1, 
          new94(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- L>=1, 
          new94(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new88(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=1, K=0, 
          new91(A,B,C,D,E,F,G,H,I,J,S,L,M,N,O,P,Q,R).
new88(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- K=< -1, 
          new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new88(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- K>=1, 
          new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new87(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=1, J=0, 
          new88(A,B,C,D,E,F,G,H,I,S,K,L,M,N,O,P,Q,R).
new87(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- J=< -1, 
          new88(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new87(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- J>=1, 
          new88(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new86(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new87(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new85(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=3, 
          new86(A,B,C,D,E,F,G,H,I,J,K,L,M,N,S,P,Q,R).
new83(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- D=0, 
          new226(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new83(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- D=< -1, 
          new227(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new83(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- D>=1, 
          new227(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W) :- X=1, F=0, 
          new74(A,B,C,D,E,F,G,H,I,J,K,L,M,N,X,Q,R,S,T,U,V,W).
new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W) :- X=0, F=< -1, 
          new74(A,B,C,D,E,F,G,H,I,J,K,L,M,N,X,Q,R,S,T,U,V,W).
new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W) :- X=0, F>=1, 
          new74(A,B,C,D,E,F,G,H,I,J,K,L,M,N,X,Q,R,S,T,U,V,W).
new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W) :- X=1, E=0, 
          new74(A,B,C,D,E,F,G,H,I,J,K,L,M,N,X,Q,R,S,T,U,V,W).
new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W) :- E=< -1, 
          new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W).
new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W) :- E>=1, 
          new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W).
new74(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- O=< -1, 
          new83(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new74(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- O>=1, 
          new83(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new74(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=2, O=0, 
          new85(A,B,C,D,E,F,G,H,I,J,K,L,M,N,W,T,U,V).
new73(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W) :- X=1, D=0, 
          new74(A,B,C,D,E,F,G,H,I,J,K,L,M,N,X,Q,R,S,T,U,V,W).
new73(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W) :- D=< -1, 
          new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W).
new73(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W) :- D>=1, 
          new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W).
new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new73(A,B,C,D,E,F,G,H,I,J,K,L,M,N,W,O,P,Q,R,S,T,U,V).
new71(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,S,T,U,V,O,P,Q,R).
new68(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=1, 
          new71(A,B,C,D,E,F,G,H,I,J,K,L,M,N,S,P,Q,R).
new65(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=2, N=1, 
          new68(A,B,C,D,E,F,G,H,I,J,K,L,M,S,O,P,Q,R).
new65(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- N=<0, 
          new68(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new65(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- N>=2, 
          new68(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=2, M=1, 
          new65(A,B,C,D,E,F,G,H,I,J,K,L,S,N,O,P,Q,R).
new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- M=<0, 
          new65(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- M>=2, 
          new65(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=2, L=1, 
          new62(A,B,C,D,E,F,G,H,I,J,K,S,M,N,O,P,Q,R).
new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- L=<0, 
          new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- L>=2, 
          new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new56(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=2, K=1, 
          new59(A,B,C,D,E,F,G,H,I,J,S,L,M,N,O,P,Q,R).
new56(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- K=<0, 
          new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new56(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- K>=2, 
          new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=2, J=1, 
          new56(A,B,C,D,E,F,G,H,I,S,K,L,M,N,O,P,Q,R).
new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- J=<0, 
          new56(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- J>=2, 
          new56(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new52(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new50(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, Q=< -1, 
          new52(A,B,C,D,E,V,G,H,I,J,K,L,M,N,R,S,T,U).
new50(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, Q>=1, 
          new52(A,B,C,D,E,V,G,H,I,J,K,L,M,N,R,S,T,U).
new50(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- Q=0, 
          new52(A,B,C,D,E,F,G,H,I,J,K,L,M,N,R,S,T,U).
new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, N=1, 
          new50(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,Q,W,S,T,U,V).
new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, N=<0, 
          new50(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,Q,W,S,T,U,V).
new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, N>=2, 
          new50(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,Q,W,S,T,U,V).
new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- C=1, 
          new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, C=<0, 
          new50(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,Q,W,S,T,U,V).
new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, C>=2, 
          new50(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,Q,W,S,T,U,V).
new45(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- 
          new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,V,O,P,Q,R,S,T,U).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, P=< -1, 
          new45(A,B,C,D,V,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, P>=1, 
          new45(A,B,C,D,V,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- P=0, 
          new45(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, M=1, 
          new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,W,R,S,T,U,V).
new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, M=<0, 
          new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,W,R,S,T,U,V).
new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, M>=2, 
          new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,W,R,S,T,U,V).
new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- B=1, 
          new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, B=<0, 
          new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,W,R,S,T,U,V).
new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, B>=2, 
          new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,W,R,S,T,U,V).
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- 
          new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N,V,O,P,Q,R,S,T,U).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, O=< -1, 
          new38(A,B,C,V,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, O>=1, 
          new38(A,B,C,V,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- O=0, 
          new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, J=1, 
          new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,W,Q,R,S,T,U,V).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, J=<0, 
          new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,W,Q,R,S,T,U,V).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, J>=2, 
          new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,W,Q,R,S,T,U,V).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- A=1, 
          new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, A=<0, 
          new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,W,Q,R,S,T,U,V).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, A>=2, 
          new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,W,Q,R,S,T,U,V).
new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- 
          new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,V,O,P,Q,R,S,T,U).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N,S,T,U,O,P,Q,R).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=1, N=0, 
          new30(A,B,C,D,E,F,G,H,I,J,K,L,M,S,O,P,Q,R).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- N=< -1, 
          new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- N>=1, 
          new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=1, M=0, 
          new27(A,B,C,D,E,F,G,H,I,J,K,L,S,N,O,P,Q,R).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- M=< -1, 
          new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- M>=1, 
          new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=1, L=0, 
          new24(A,B,C,D,E,F,G,H,I,J,K,S,M,N,O,P,Q,R).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- L=< -1, 
          new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- L>=1, 
          new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=1, K=0, 
          new21(A,B,C,D,E,F,G,H,I,J,S,L,M,N,O,P,Q,R).
new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- K=< -1, 
          new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- K>=1, 
          new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=1, J=0, 
          new18(A,B,C,D,E,F,G,H,I,S,K,L,M,N,O,P,Q,R).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- J=< -1, 
          new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- J>=1, 
          new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=0, I=1, 
          new14(A,B,C,D,E,S,G,H,I,J,K,L,M,N,O,P,Q,R).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=2, I=<0, 
          new14(A,B,C,D,E,S,G,H,I,J,K,L,M,N,O,P,Q,R).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=2, I>=2, 
          new14(A,B,C,D,E,S,G,H,I,J,K,L,M,N,O,P,Q,R).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=0, H=1, 
          new11(A,B,C,D,S,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=2, H=<0, 
          new11(A,B,C,D,S,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=2, H>=2, 
          new11(A,B,C,D,S,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=0, G=1, 
          new8(A,B,C,S,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=2, G=<0, 
          new8(A,B,C,S,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=2, G>=2, 
          new8(A,B,C,S,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=0, 
          new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,Q,R,O).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=1, Q=1, R=1, 
          new4(A,B,C,D,E,F,P,Q,R,J,K,L,M,N,O).
new2(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new1 :- A=2, B=2, C=2, D=2, E=2, F=0, G=0, H=0, 
          new2(H,G,F,I,J,K,L,M,N,E,D,C,B,A).
false :- new1.
