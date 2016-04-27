new47(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- R=<0.
new47(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- R>=2.
new47(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, R=1, 
          new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,T,S).
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- P=< -1, 
          new47(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- P>=1, 
          new47(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- P=0, 
          new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- O=<0.
new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- O>=2.
new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, O=1, 
          new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,T,P,Q,R,S).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- M=< -1, 
          new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- M>=1, 
          new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- M=0, 
          new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- L=<0.
new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- L>=2.
new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, L=1, 
          new43(A,B,C,D,E,F,G,H,I,J,K,T,M,N,O,P,Q,R,S).
new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- J=< -1, 
          new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- J>=1, 
          new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- J=0, 
          new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- I=<0.
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- I>=2.
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, I=1, 
          new40(A,B,C,D,E,F,G,H,T,J,K,L,M,N,O,P,Q,R,S).
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- G=< -1, 
          new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- G>=1, 
          new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- G=0, 
          new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- F=<0.
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- F>=2.
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, F=1, 
          new37(A,B,C,D,E,T,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- D=< -1, 
          new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- D>=1, 
          new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- D=0, 
          new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- C=<0.
new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- C>=2.
new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, C=1, 
          new34(A,B,T,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- A=< -1, 
          new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- A>=1, 
          new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- A=0, 
          new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, P=< -1, 
          new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,T,S).
new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, P>=1, 
          new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,T,S).
new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- P=0, 
          new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, M=< -1, 
          new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N,T,P,Q,R,S).
new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, M>=1, 
          new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N,T,P,Q,R,S).
new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- M=0, 
          new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, J=< -1, 
          new23(A,B,C,D,E,F,G,H,I,J,K,T,M,N,O,P,Q,R,S).
new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, J>=1, 
          new23(A,B,C,D,E,F,G,H,I,J,K,T,M,N,O,P,Q,R,S).
new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- J=0, 
          new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, G=< -1, 
          new20(A,B,C,D,E,F,G,H,T,J,K,L,M,N,O,P,Q,R,S).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, G>=1, 
          new20(A,B,C,D,E,F,G,H,T,J,K,L,M,N,O,P,Q,R,S).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- G=0, 
          new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, D=< -1, 
          new17(A,B,C,D,E,T,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, D>=1, 
          new17(A,B,C,D,E,T,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- D=0, 
          new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, A=< -1, 
          new14(A,B,T,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=1, A>=1, 
          new14(A,B,T,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- A=0, 
          new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, U=0, V=0, W=0, X=0, Y=0, 
          S=< -1, new12(A,B,U,D,E,V,G,H,W,J,K,X,M,N,Y,P,Q,T,S).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- T=0, U=0, V=0, W=0, X=0, Y=0, 
          S>=1, new12(A,B,U,D,E,V,G,H,W,J,K,X,M,N,Y,P,Q,T,S).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- 
          new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,T).
new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- 
          new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- 
          new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,T,T,R,S).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- 
          new8(A,B,C,D,E,F,G,H,I,J,K,L,T,T,O,P,Q,R,S).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- 
          new7(A,B,C,D,E,F,G,H,I,T,T,L,M,N,O,P,Q,R,S).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- 
          new6(A,B,C,D,E,F,T,T,I,J,K,L,M,N,O,P,Q,R,S).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- 
          new5(A,B,C,T,T,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :- 
          new4(T,T,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new2 :- new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).
new1 :- new2.
false :- new1.
