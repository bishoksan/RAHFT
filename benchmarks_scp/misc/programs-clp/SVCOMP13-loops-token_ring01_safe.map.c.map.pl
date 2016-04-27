new239(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1, W=2, X=0, Y=2, 
          N=< -1, new173(V,B,W,X,E,F,G,H,I,Y,K,L,O,P,Q,R,S,T,U).
new239(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1, W=2, X=0, Y=2, N>=1, 
          new173(V,B,W,X,E,F,G,H,I,Y,K,L,O,P,Q,R,S,T,U).
new239(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1, W=2, X=2, N=0, 
          new173(V,B,W,D,E,F,G,H,I,X,K,L,O,P,Q,R,S,T,U).
new238(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, J=1, 
          new239(A,B,C,D,E,F,G,H,I,J,K,L,N,W,P,Q,R,S,T,U,V).
new238(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, J=<0, 
          new239(A,B,C,D,E,F,G,H,I,J,K,L,N,W,P,Q,R,S,T,U,V).
new238(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, J>=2, 
          new239(A,B,C,D,E,F,G,H,I,J,K,L,N,W,P,Q,R,S,T,U,V).
new237(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- B=1, 
          new238(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new237(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, B=<0, 
          new239(A,B,C,D,E,F,G,H,I,J,K,L,N,W,P,Q,R,S,T,U,V).
new237(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, B>=2, 
          new239(A,B,C,D,E,F,G,H,I,J,K,L,N,W,P,Q,R,S,T,U,V).
new234(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- 
          new237(A,B,C,D,E,F,G,H,I,J,K,L,V,M,N,O,P,Q,R,S,T,U).
new232(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, M=< -1, 
          new234(A,B,V,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new232(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, M>=1, 
          new234(A,B,V,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new232(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- M=0, 
          new234(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new231(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, I=1, 
          new232(A,B,C,D,E,F,G,H,I,J,K,L,W,O,P,Q,R,S,T,U,V).
new231(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, I=<0, 
          new232(A,B,C,D,E,F,G,H,I,J,K,L,W,O,P,Q,R,S,T,U,V).
new231(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, I>=2, 
          new232(A,B,C,D,E,F,G,H,I,J,K,L,W,O,P,Q,R,S,T,U,V).
new230(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- A=1, 
          new231(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new230(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, A=<0, 
          new232(A,B,C,D,E,F,G,H,I,J,K,L,W,O,P,Q,R,S,T,U,V).
new230(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, A>=2, 
          new232(A,B,C,D,E,F,G,H,I,J,K,L,W,O,P,Q,R,S,T,U,V).
new229(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- 
          new230(A,B,C,D,E,F,G,H,I,J,K,L,V,M,N,O,P,Q,R,S,T,U).
new228(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- 
          new229(A,B,C,D,E,F,G,H,I,J,K,L,T,U,M,N,O,P,Q,R,S).
new227(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- 
          new228(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new226(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, 
          new227(A,B,C,D,E,F,G,H,I,T,U,U,M,N,O,P,Q,R,S).
new223(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new220(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- K-L=<0, 
          new223(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new220(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- K-L>=2, 
          new223(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new220(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- K=1+L, 
          new217(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new218(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- A=1, 
          new220(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new218(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- A=<0, 
          new217(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new218(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- A>=2, 
          new217(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new217(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- 
          new226(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new216(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- A=0, 
          new217(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new216(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- A=< -1, 
          new218(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new216(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- A>=1, 
          new218(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new213(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- 
          new216(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new212(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, N=< -1, 
          new213(A,B,T,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new212(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, N>=1, 
          new213(A,B,T,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new212(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- N=0, 
          new173(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new200(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2, W=0, N=< -1, 
          new183(A,B,C,W,E,F,G,H,V,J,K,L,O,P,Q,R,S,T,U).
new200(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2, W=0, N>=1, 
          new183(A,B,C,W,E,F,G,H,V,J,K,L,O,P,Q,R,S,T,U).
new200(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2, N=0, 
          new183(A,B,C,D,E,F,G,H,V,J,K,L,O,P,Q,R,S,T,U).
new199(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, J=1, 
          new200(A,B,C,D,E,F,G,H,I,J,K,L,N,W,P,Q,R,S,T,U,V).
new199(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, J=<0, 
          new200(A,B,C,D,E,F,G,H,I,J,K,L,N,W,P,Q,R,S,T,U,V).
new199(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, J>=2, 
          new200(A,B,C,D,E,F,G,H,I,J,K,L,N,W,P,Q,R,S,T,U,V).
new198(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- B=1, 
          new199(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new198(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, B=<0, 
          new200(A,B,C,D,E,F,G,H,I,J,K,L,N,W,P,Q,R,S,T,U,V).
new198(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, B>=2, 
          new200(A,B,C,D,E,F,G,H,I,J,K,L,N,W,P,Q,R,S,T,U,V).
new195(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- 
          new198(A,B,C,D,E,F,G,H,I,J,K,L,V,M,N,O,P,Q,R,S,T,U).
new193(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, M=< -1, 
          new195(A,B,V,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new193(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, M>=1, 
          new195(A,B,V,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new193(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- M=0, 
          new195(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new192(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=1, I=1, 
          new193(A,B,C,D,E,F,G,H,I,J,K,L,W,O,P,Q,R,S,T,U,V).
new192(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, I=<0, 
          new193(A,B,C,D,E,F,G,H,I,J,K,L,W,O,P,Q,R,S,T,U,V).
new192(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, I>=2, 
          new193(A,B,C,D,E,F,G,H,I,J,K,L,W,O,P,Q,R,S,T,U,V).
new191(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- A=1, 
          new192(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new191(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, A=<0, 
          new193(A,B,C,D,E,F,G,H,I,J,K,L,W,O,P,Q,R,S,T,U,V).
new191(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W=0, A>=2, 
          new193(A,B,C,D,E,F,G,H,I,J,K,L,W,O,P,Q,R,S,T,U,V).
new190(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- 
          new191(A,B,C,D,E,F,G,H,I,J,K,L,V,M,N,O,P,Q,R,S,T,U).
new189(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- 
          new190(A,B,C,D,E,F,G,H,I,J,K,L,T,U,M,N,O,P,Q,R,S).
new186(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- 
          new189(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new184(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, U=1+K, B=1, 
          new186(A,B,C,D,E,F,G,H,T,J,U,L,M,N,O,P,Q,R,S).
new184(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- B=<0, 
          new183(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new184(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- B>=2, 
          new183(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new183(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, U=2, 
          new56(A,T,C,U,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new182(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- B=0, 
          new183(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new182(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- B=< -1, 
          new184(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new182(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- B>=1, 
          new184(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new179(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- 
          new182(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new178(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, O=< -1, 
          new179(A,B,C,T,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new178(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, O>=1, 
          new179(A,B,C,T,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new178(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- O=0, 
          new56(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new175(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- 
          new178(A,B,C,D,E,F,G,H,I,J,K,L,M,N,T,P,Q,R,S).
new173(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- D=0, 
          new175(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new173(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- D=< -1, 
          new56(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new173(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- D>=1, 
          new56(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new172(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- 
          new212(A,B,C,D,E,F,G,H,I,J,K,L,M,T,O,P,Q,R,S).
new154(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=2, J=1, 
          new117(A,B,C,D,E,F,G,H,I,Q,K,L,M,N,O,P).
new154(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- J=<0, 
          new117(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new154(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- J>=2, 
          new117(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new151(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=2, I=1, 
          new154(A,B,C,D,E,F,G,H,Q,J,K,L,M,N,O,P).
new151(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- I=<0, 
          new154(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new151(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- I>=2, 
          new154(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new148(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=2, H=1, 
          new151(A,B,C,D,E,F,G,Q,I,J,K,L,M,N,O,P).
new148(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- H=<0, 
          new151(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new148(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- H>=2, 
          new151(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new147(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=2, G=1, 
          new148(A,B,C,D,E,F,Q,H,I,J,K,L,M,N,O,P).
new147(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- G=<0, 
          new148(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new147(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- G>=2, 
          new148(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new144(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- 
          new147(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new142(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=0, N=< -1, 
          new144(A,B,C,S,E,F,G,H,I,J,K,L,O,P,Q,R).
new142(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=0, N>=1, 
          new144(A,B,C,S,E,F,G,H,I,J,K,L,O,P,Q,R).
new142(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- N=0, 
          new144(A,B,C,D,E,F,G,H,I,J,K,L,O,P,Q,R).
new141(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, J=1, 
          new142(A,B,C,D,E,F,G,H,I,J,K,L,N,T,P,Q,R,S).
new141(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, J=<0, 
          new142(A,B,C,D,E,F,G,H,I,J,K,L,N,T,P,Q,R,S).
new141(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, J>=2, 
          new142(A,B,C,D,E,F,G,H,I,J,K,L,N,T,P,Q,R,S).
new140(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- B=1, 
          new141(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new140(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, B=<0, 
          new142(A,B,C,D,E,F,G,H,I,J,K,L,N,T,P,Q,R,S).
new140(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, B>=2, 
          new142(A,B,C,D,E,F,G,H,I,J,K,L,N,T,P,Q,R,S).
new137(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new140(A,B,C,D,E,F,G,H,I,J,K,L,S,M,N,O,P,Q,R).
new135(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=0, M=< -1, 
          new137(A,B,S,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new135(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=0, M>=1, 
          new137(A,B,S,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new135(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- M=0, 
          new137(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new134(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, I=1, 
          new135(A,B,C,D,E,F,G,H,I,J,K,L,T,O,P,Q,R,S).
new134(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, I=<0, 
          new135(A,B,C,D,E,F,G,H,I,J,K,L,T,O,P,Q,R,S).
new134(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, I>=2, 
          new135(A,B,C,D,E,F,G,H,I,J,K,L,T,O,P,Q,R,S).
new133(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- A=1, 
          new134(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new133(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, A=<0, 
          new135(A,B,C,D,E,F,G,H,I,J,K,L,T,O,P,Q,R,S).
new133(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, A>=2, 
          new135(A,B,C,D,E,F,G,H,I,J,K,L,T,O,P,Q,R,S).
new132(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new133(A,B,C,D,E,F,G,H,I,J,K,L,S,M,N,O,P,Q,R).
new131(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- 
          new132(A,B,C,D,E,F,G,H,I,J,K,L,Q,R,M,N,O,P).
new127(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- O=0, 
          new52(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new122(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, D=0, 
          new121(A,B,C,D,E,F,G,H,I,J,K,L,T,O,P,Q,R,S).
new122(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, D=< -1, 
          new121(A,B,C,D,E,F,G,H,I,J,K,L,T,O,P,Q,R,S).
new122(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, D>=1, 
          new121(A,B,C,D,E,F,G,H,I,J,K,L,T,O,P,Q,R,S).
new121(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=0, M=< -1, 
          new127(A,B,C,D,E,F,G,H,I,J,K,L,O,P,S,R).
new121(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=0, M>=1, 
          new127(A,B,C,D,E,F,G,H,I,J,K,L,O,P,S,R).
new121(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=1, M=0, 
          new127(A,B,C,D,E,F,G,H,I,J,K,L,O,P,S,R).
new120(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, C=0, 
          new121(A,B,C,D,E,F,G,H,I,J,K,L,T,O,P,Q,R,S).
new120(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- C=< -1, 
          new122(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new120(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- C>=1, 
          new122(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new119(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new120(A,B,C,D,E,F,G,H,I,J,K,L,S,M,N,O,P,Q,R).
new117(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- 
          new119(A,B,C,D,E,F,G,H,I,J,K,L,Q,R,M,N,O,P).
new116(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=1, 
          new131(A,B,C,D,E,F,Q,H,I,J,K,L,M,N,O,P).
new111(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=1, D=0, 
          new110(A,B,C,D,E,F,G,H,I,J,K,L,N,R,P,Q).
new111(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=0, D=< -1, 
          new110(A,B,C,D,E,F,G,H,I,J,K,L,N,R,P,Q).
new111(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=0, D>=1, 
          new110(A,B,C,D,E,F,G,H,I,J,K,L,N,R,P,Q).
new110(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=4, N=0, 
          new116(A,B,C,D,E,F,G,H,I,J,K,L,Q,N,O,P).
new110(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- N=< -1, 
          new117(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new110(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- N>=1, 
          new117(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new109(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=1, C=0, 
          new110(A,B,C,D,E,F,G,H,I,J,K,L,N,R,P,Q).
new109(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- C=< -1, 
          new111(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new109(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- C>=1, 
          new111(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new106(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- 
          new109(A,B,C,D,E,F,G,H,I,J,K,L,Q,M,N,O,P).
new103(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=2, J=1, 
          new106(A,B,C,D,E,F,G,H,I,Q,K,L,M,N,O,P).
new103(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- J=<0, 
          new106(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new103(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- J>=2, 
          new106(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new100(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=2, I=1, 
          new103(A,B,C,D,E,F,G,H,Q,J,K,L,M,N,O,P).
new100(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- I=<0, 
          new103(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new100(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- I>=2, 
          new103(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new97(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=2, H=1, 
          new100(A,B,C,D,E,F,G,Q,I,J,K,L,M,N,O,P).
new97(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- H=<0, 
          new100(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new97(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- H>=2, 
          new100(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new96(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=2, G=1, 
          new97(A,B,C,D,E,F,Q,H,I,J,K,L,M,N,O,P).
new96(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- G=<0, 
          new97(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new96(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- G>=2, 
          new97(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new93(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- 
          new96(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=0, N=< -1, 
          new93(A,B,C,S,E,F,G,H,I,J,K,L,O,P,Q,R).
new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=0, N>=1, 
          new93(A,B,C,S,E,F,G,H,I,J,K,L,O,P,Q,R).
new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- N=0, 
          new93(A,B,C,D,E,F,G,H,I,J,K,L,O,P,Q,R).
new90(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, J=1, 
          new91(A,B,C,D,E,F,G,H,I,J,K,L,N,T,P,Q,R,S).
new90(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, J=<0, 
          new91(A,B,C,D,E,F,G,H,I,J,K,L,N,T,P,Q,R,S).
new90(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, J>=2, 
          new91(A,B,C,D,E,F,G,H,I,J,K,L,N,T,P,Q,R,S).
new89(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- B=1, 
          new90(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new89(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, B=<0, 
          new91(A,B,C,D,E,F,G,H,I,J,K,L,N,T,P,Q,R,S).
new89(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, B>=2, 
          new91(A,B,C,D,E,F,G,H,I,J,K,L,N,T,P,Q,R,S).
new86(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new89(A,B,C,D,E,F,G,H,I,J,K,L,S,M,N,O,P,Q,R).
new84(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=0, M=< -1, 
          new86(A,B,S,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new84(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=0, M>=1, 
          new86(A,B,S,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new84(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- M=0, 
          new86(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new83(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, I=1, 
          new84(A,B,C,D,E,F,G,H,I,J,K,L,T,O,P,Q,R,S).
new83(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, I=<0, 
          new84(A,B,C,D,E,F,G,H,I,J,K,L,T,O,P,Q,R,S).
new83(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, I>=2, 
          new84(A,B,C,D,E,F,G,H,I,J,K,L,T,O,P,Q,R,S).
new82(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- A=1, 
          new83(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new82(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, A=<0, 
          new84(A,B,C,D,E,F,G,H,I,J,K,L,T,O,P,Q,R,S).
new82(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, A>=2, 
          new84(A,B,C,D,E,F,G,H,I,J,K,L,T,O,P,Q,R,S).
new81(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new82(A,B,C,D,E,F,G,H,I,J,K,L,S,M,N,O,P,Q,R).
new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- 
          new81(A,B,C,D,E,F,G,H,I,J,K,L,Q,R,M,N,O,P).
new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=1, J=0, 
          new78(A,B,C,D,E,F,G,H,I,Q,K,L,M,N,O,P).
new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- J=< -1, 
          new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- J>=1, 
          new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=1, I=0, 
          new75(A,B,C,D,E,F,G,H,Q,J,K,L,M,N,O,P).
new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- I=< -1, 
          new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- I>=1, 
          new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new69(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=1, H=0, 
          new72(A,B,C,D,E,F,G,Q,I,J,K,L,M,N,O,P).
new69(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- H=< -1, 
          new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new69(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- H>=1, 
          new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new68(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=1, G=0, 
          new69(A,B,C,D,E,F,Q,H,I,J,K,L,M,N,O,P).
new68(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- G=< -1, 
          new69(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new68(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- G>=1, 
          new69(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new67(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- 
          new68(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new66(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=3, 
          new67(A,B,C,D,E,F,G,H,I,J,K,L,Q,N,O,P).
new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- C=0, 
          new172(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- C=< -1, 
          new173(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- C>=1, 
          new173(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=1, D=0, 
          new58(A,B,C,D,E,F,G,H,I,J,K,L,U,O,P,Q,R,S,T).
new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=0, D=< -1, 
          new58(A,B,C,D,E,F,G,H,I,J,K,L,U,O,P,Q,R,S,T).
new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=0, D>=1, 
          new58(A,B,C,D,E,F,G,H,I,J,K,L,U,O,P,Q,R,S,T).
new58(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- M=< -1, 
          new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new58(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- M>=1, 
          new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new58(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=2, M=0, 
          new66(A,B,C,D,E,F,G,H,I,J,K,L,T,Q,R,S).
new57(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=1, C=0, 
          new58(A,B,C,D,E,F,G,H,I,J,K,L,U,O,P,Q,R,S,T).
new57(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- C=< -1, 
          new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new57(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- C>=1, 
          new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new56(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- 
          new57(A,B,C,D,E,F,G,H,I,J,K,L,T,M,N,O,P,Q,R,S).
new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- 
          new56(A,B,C,D,E,F,G,H,I,J,K,L,Q,R,S,M,N,O,P).
new52(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=1, 
          new55(A,B,C,D,E,F,G,H,I,J,K,L,Q,N,O,P).
new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=2, J=1, 
          new52(A,B,C,D,E,F,G,H,I,Q,K,L,M,N,O,P).
new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- J=<0, 
          new52(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- J>=2, 
          new52(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=2, I=1, 
          new49(A,B,C,D,E,F,G,H,Q,J,K,L,M,N,O,P).
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- I=<0, 
          new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- I>=2, 
          new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=2, H=1, 
          new46(A,B,C,D,E,F,G,Q,I,J,K,L,M,N,O,P).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- H=<0, 
          new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- H>=2, 
          new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=2, G=1, 
          new43(A,B,C,D,E,F,Q,H,I,J,K,L,M,N,O,P).
new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- G=<0, 
          new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- G>=2, 
          new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- 
          new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=0, N=< -1, 
          new39(A,B,C,S,E,F,G,H,I,J,K,L,O,P,Q,R).
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=0, N>=1, 
          new39(A,B,C,S,E,F,G,H,I,J,K,L,O,P,Q,R).
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- N=0, 
          new39(A,B,C,D,E,F,G,H,I,J,K,L,O,P,Q,R).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, J=1, 
          new37(A,B,C,D,E,F,G,H,I,J,K,L,N,T,P,Q,R,S).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, J=<0, 
          new37(A,B,C,D,E,F,G,H,I,J,K,L,N,T,P,Q,R,S).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, J>=2, 
          new37(A,B,C,D,E,F,G,H,I,J,K,L,N,T,P,Q,R,S).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- B=1, 
          new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, B=<0, 
          new37(A,B,C,D,E,F,G,H,I,J,K,L,N,T,P,Q,R,S).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, B>=2, 
          new37(A,B,C,D,E,F,G,H,I,J,K,L,N,T,P,Q,R,S).
new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new35(A,B,C,D,E,F,G,H,I,J,K,L,S,M,N,O,P,Q,R).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=0, M=< -1, 
          new32(A,B,S,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=0, M>=1, 
          new32(A,B,S,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- M=0, 
          new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, I=1, 
          new30(A,B,C,D,E,F,G,H,I,J,K,L,T,O,P,Q,R,S).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, I=<0, 
          new30(A,B,C,D,E,F,G,H,I,J,K,L,T,O,P,Q,R,S).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, I>=2, 
          new30(A,B,C,D,E,F,G,H,I,J,K,L,T,O,P,Q,R,S).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- A=1, 
          new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, A=<0, 
          new30(A,B,C,D,E,F,G,H,I,J,K,L,T,O,P,Q,R,S).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, A>=2, 
          new30(A,B,C,D,E,F,G,H,I,J,K,L,T,O,P,Q,R,S).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new28(A,B,C,D,E,F,G,H,I,J,K,L,S,M,N,O,P,Q,R).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- 
          new27(A,B,C,D,E,F,G,H,I,J,K,L,Q,R,M,N,O,P).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=1, J=0, 
          new24(A,B,C,D,E,F,G,H,I,Q,K,L,M,N,O,P).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- J=< -1, 
          new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- J>=1, 
          new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=1, I=0, 
          new21(A,B,C,D,E,F,G,H,Q,J,K,L,M,N,O,P).
new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- I=< -1, 
          new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- I>=1, 
          new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=1, H=0, 
          new18(A,B,C,D,E,F,G,Q,I,J,K,L,M,N,O,P).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- H=< -1, 
          new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- H>=1, 
          new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=1, G=0, 
          new15(A,B,C,D,E,F,Q,H,I,J,K,L,M,N,O,P).
new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- G=< -1, 
          new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- G>=1, 
          new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- 
          new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=0, F=1, 
          new11(A,B,C,Q,E,F,G,H,I,J,K,L,M,N,O,P).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=2, F=<0, 
          new11(A,B,C,Q,E,F,G,H,I,J,K,L,M,N,O,P).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=2, F>=2, 
          new11(A,B,C,Q,E,F,G,H,I,J,K,L,M,N,O,P).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=0, E=1, 
          new8(A,B,Q,D,E,F,G,H,I,J,K,L,M,N,O,P).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=2, E=<0, 
          new8(A,B,Q,D,E,F,G,H,I,J,K,L,M,N,O,P).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=2, E>=2, 
          new8(A,B,Q,D,E,F,G,H,I,J,K,L,M,N,O,P).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=0, new5(A,B,C,D,E,F,G,H,I,J,K,L,N,O,P,M).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1, O=1, new4(A,B,C,D,N,O,G,H,I,J,K,L,M).
new2(A,B,C,D,E,F,G,H,I,J,K,L) :- new3(A,B,C,D,E,F,G,H,I,J,K,L,M).
new1 :- A=2, B=2, C=2, D=2, E=0, F=0, new2(F,E,G,H,I,J,D,C,B,A,K,L).
false :- new1.
