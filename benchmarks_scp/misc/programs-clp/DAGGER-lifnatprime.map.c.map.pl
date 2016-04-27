new54(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=1+C+D, S=0, T=0, U= -1+E, E>=1, 
          new4(A,R,S,T,U,F,G,H,I,J,K,L,M,N,O,P,Q).
new52(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R= -1+A, S=1+B+C+D, T=0, U=0, A>=1, 
          new4(R,S,T,U,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new51(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- H=< -1, 
          new52(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new51(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- H>=1, 
          new52(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new51(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- H=0, 
          new54(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new50(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- 
          new51(A,B,C,D,E,F,G,R,I,J,K,L,M,N,O,P,Q).
new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R= -1+A, S=1+E+F+G, T=0, U=0, A>=1, 
          new4(R,B,C,D,S,T,U,H,I,J,K,L,M,N,O,P,Q).
new47(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- I=< -1, 
          new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new47(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- I>=1, 
          new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new47(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- I=0, 
          new50(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- 
          new47(A,B,C,D,E,F,G,H,R,J,K,L,M,N,O,P,Q).
new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R= -1+B, S=1+E+F+G, T=0, U=0, B>=1, 
          new4(A,R,C,D,S,T,U,H,I,J,K,L,M,N,O,P,Q).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- J=< -1, 
          new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- J>=1, 
          new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- J=0, 
          new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- 
          new43(A,B,C,D,E,F,G,H,I,R,K,L,M,N,O,P,Q).
new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=A+B+C+D, S=0, T=1, U=0, B>=1, 
          new4(R,S,T,U,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- K=< -1, 
          new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- K>=1, 
          new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- K=0, 
          new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- 
          new39(A,B,C,D,E,F,G,H,I,J,R,L,M,N,O,P,Q).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=A+B+C+D, S=0, T=1, U=0, V= -1+E, 
          E>=1, new4(R,S,T,U,V,F,G,H,I,J,K,L,M,N,O,P,Q).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- L=< -1, 
          new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- L>=1, 
          new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- L=0, 
          new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- 
          new35(A,B,C,D,E,F,G,H,I,J,K,R,M,N,O,P,Q).
new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=A+E+F+G, S=0, T=1, U=0, E>=1, 
          new4(R,B,C,D,S,T,U,H,I,J,K,L,M,N,O,P,Q).
new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- M=< -1, 
          new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- M>=1, 
          new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- M=0, 
          new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- 
          new31(A,B,C,D,E,F,G,H,I,J,K,L,R,N,O,P,Q).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=A+E+F+G, S= -1+B, T=0, U=1, V=0, 
          B>=1, new4(R,S,C,D,T,U,V,H,I,J,K,L,M,N,O,P,Q).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- N=< -1, 
          new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- N>=1, 
          new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- N=0, 
          new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- 
          new27(A,B,C,D,E,F,G,H,I,J,K,L,M,R,O,P,Q).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R= -1+C, S=1+D, C>=1, 
          new4(A,B,R,S,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- O=< -1, 
          new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- O>=1, 
          new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- O=0, 
          new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- 
          new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,R,P,Q).
new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R= -1+F, S=1+G, F>=1, 
          new4(A,B,C,D,E,R,S,H,I,J,K,L,M,N,O,P,Q).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- P=< -1, 
          new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- P>=1, 
          new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- P=0, 
          new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- A=0.
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=1, A+B+C+D+E+F+G>=1, 
          new17(R,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=0, A+B+C+D+E+F+G=<0, 
          new17(R,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- A=0.
new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- A=< -1, 
          new15(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- A>=1, 
          new15(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=1, F+G=<1, 
          new13(R,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=0, F+G>=2, 
          new13(R,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- A=0.
new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- A=< -1, 
          new11(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- A>=1, 
          new11(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=1, C+D=<1, 
          new9(R,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=0, C+D>=2, 
          new9(R,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- 
          new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,R,Q).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- Q=< -1, 
          new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- Q>=1, 
          new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- Q=0, 
          new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- 
          new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,R).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=0, S=0, T=0, U=0, V=0, W=0, A>=1, 
          new4(A,R,S,T,U,V,W,H,I,J,K,L,M,N,O,P,Q).
new2 :- new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new1 :- new2.
false :- new1.
