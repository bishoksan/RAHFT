new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- O=<0.
new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- O>=2.
new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=0, O=1, 
          new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,Q,P).
new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- M=< -1, 
          new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- M>=1, 
          new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- M=0, 
          new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- L=<0.
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- L>=2.
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=0, L=1, 
          new39(A,B,C,D,E,F,G,H,I,J,K,Q,M,N,O,P).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- J=< -1, 
          new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- J>=1, 
          new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- J=0, 
          new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- I=<0.
new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- I>=2.
new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=0, I=1, 
          new36(A,B,C,D,E,F,G,H,Q,J,K,L,M,N,O,P).
new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- G=< -1, 
          new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- G>=1, 
          new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- G=0, 
          new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- F=<0.
new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- F>=2.
new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=0, F=1, 
          new33(A,B,C,D,E,Q,G,H,I,J,K,L,M,N,O,P).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- D=< -1, 
          new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- D>=1, 
          new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- D=0, 
          new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- C=<0.
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- C>=2.
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=0, C=1, 
          new30(A,B,Q,D,E,F,G,H,I,J,K,L,M,N,O,P).
new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=< -1, 
          new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A>=1, 
          new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=0, 
          new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=1, M=< -1, 
          new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N,Q,P).
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=1, M>=1, 
          new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N,Q,P).
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- M=0, 
          new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=1, J=< -1, 
          new22(A,B,C,D,E,F,G,H,I,J,K,Q,M,N,O,P).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=1, J>=1, 
          new22(A,B,C,D,E,F,G,H,I,J,K,Q,M,N,O,P).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- J=0, 
          new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=1, G=< -1, 
          new19(A,B,C,D,E,F,G,H,Q,J,K,L,M,N,O,P).
new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=1, G>=1, 
          new19(A,B,C,D,E,F,G,H,Q,J,K,L,M,N,O,P).
new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- G=0, 
          new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=1, D=< -1, 
          new16(A,B,C,D,E,Q,G,H,I,J,K,L,M,N,O,P).
new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=1, D>=1, 
          new16(A,B,C,D,E,Q,G,H,I,J,K,L,M,N,O,P).
new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- D=0, 
          new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=1, A=< -1, 
          new13(A,B,Q,D,E,F,G,H,I,J,K,L,M,N,O,P).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=1, A>=1, 
          new13(A,B,Q,D,E,F,G,H,I,J,K,L,M,N,O,P).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=0, 
          new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=0, R=0, S=0, T=0, U=0, P=< -1, 
          new11(A,B,R,D,E,S,G,H,T,J,K,U,M,N,Q,P).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=0, R=0, S=0, T=0, U=0, P>=1, 
          new11(A,B,R,D,E,S,G,H,T,J,K,U,M,N,Q,P).
new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- new8(A,B,C,D,E,F,G,H,I,J,K,L,Q,Q,O,P).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- new7(A,B,C,D,E,F,G,H,I,Q,Q,L,M,N,O,P).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- new6(A,B,C,D,E,F,Q,Q,I,J,K,L,M,N,O,P).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- new5(A,B,C,Q,Q,F,G,H,I,J,K,L,M,N,O,P).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- new4(Q,Q,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new2 :- new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new1 :- new2.
false :- new1.
