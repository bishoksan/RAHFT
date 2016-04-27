new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- A=0.
new61(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1, A=<40, 
          new64(V,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new61(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=0, A>=41, 
          new64(V,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new58(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1+A, U=0, 
          new61(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new58(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, U=< -1, 
          new61(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new58(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, U>=1, 
          new61(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1+A, T=0, 
          new58(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, T=< -1, 
          new58(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, T>=1, 
          new58(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new52(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1+A, S=0, 
          new55(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new52(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, S=< -1, 
          new55(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new52(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, S>=1, 
          new55(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1+A, R=0, 
          new52(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, R=< -1, 
          new52(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, R>=1, 
          new52(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1+A, Q=0, 
          new49(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, Q=< -1, 
          new49(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, Q>=1, 
          new49(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1+A, P=0, 
          new46(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, P=< -1, 
          new46(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, P>=1, 
          new46(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1+A, O=0, 
          new43(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, O=< -1, 
          new43(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, O>=1, 
          new43(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1+A, N=0, 
          new40(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, N=< -1, 
          new40(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, N>=1, 
          new40(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1+A, M=0, 
          new37(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, M=< -1, 
          new37(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, M>=1, 
          new37(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1+A, L=0, 
          new34(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, L=< -1, 
          new34(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, L>=1, 
          new34(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1+A, K=0, 
          new31(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, K=< -1, 
          new31(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, K>=1, 
          new31(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1+A, J=0, 
          new28(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, J=< -1, 
          new28(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, J>=1, 
          new28(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1+A, I=0, 
          new25(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, I=< -1, 
          new25(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, I>=1, 
          new25(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1+A, H=0, 
          new22(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, H=< -1, 
          new22(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, H>=1, 
          new22(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1+A, G=0, 
          new19(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, G=< -1, 
          new19(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, G>=1, 
          new19(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1+A, F=0, 
          new16(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, F=< -1, 
          new16(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, F>=1, 
          new16(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1+A, E=0, 
          new13(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, E=< -1, 
          new13(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, E>=1, 
          new13(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1+A, D=0, 
          new10(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, D=< -1, 
          new10(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, D>=1, 
          new10(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1+A, C=0, 
          new7(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, C=< -1, 
          new7(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, C>=1, 
          new7(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=1+A, B=0, 
          new4(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, B=< -1, 
          new4(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- V=2+A, B>=1, 
          new4(V,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new2 :- A=0, new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).
new1 :- new2.
false :- new1.
