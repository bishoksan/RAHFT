new82(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=1+C+D, S=0, T=0, U= -1+E, E>=1, 
          new4(A,R,S,T,U,F,G,H,I,J,K,L,M,N,O,P,Q).
new80(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R= -1+A, S=1+B+C+D, T=0, U=0, A>=1, 
          new4(R,S,T,U,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new79(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- H=< -1, 
          new80(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new79(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- H>=1, 
          new80(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new79(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- H=0, 
          new82(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- 
          new79(A,B,C,D,E,F,G,R,I,J,K,L,M,N,O,P,Q).
new76(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R= -1+A, S=1+E+F+G, T=0, U=0, A>=1, 
          new4(R,B,C,D,S,T,U,H,I,J,K,L,M,N,O,P,Q).
new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- I=< -1, 
          new76(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- I>=1, 
          new76(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- I=0, 
          new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new74(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- 
          new75(A,B,C,D,E,F,G,H,R,J,K,L,M,N,O,P,Q).
new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R= -1+B, S=1+E+F+G, T=0, U=0, B>=1, 
          new4(A,R,C,D,S,T,U,H,I,J,K,L,M,N,O,P,Q).
new71(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- J=< -1, 
          new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new71(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- J>=1, 
          new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new71(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- J=0, 
          new74(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new70(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- 
          new71(A,B,C,D,E,F,G,H,I,R,K,L,M,N,O,P,Q).
new68(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=A+B+C+D, S=0, T=1, U=0, B>=1, 
          new4(R,S,T,U,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new67(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- K=< -1, 
          new68(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new67(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- K>=1, 
          new68(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new67(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- K=0, 
          new70(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new66(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- 
          new67(A,B,C,D,E,F,G,H,I,J,R,L,M,N,O,P,Q).
new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=A+B+C+D, S=0, T=1, U=0, V= -1+E, 
          E>=1, new4(R,S,T,U,V,F,G,H,I,J,K,L,M,N,O,P,Q).
new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- L=< -1, 
          new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- L>=1, 
          new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- L=0, 
          new66(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- 
          new63(A,B,C,D,E,F,G,H,I,J,K,R,M,N,O,P,Q).
new60(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=A+E+F+G, S=0, T=1, U=0, E>=1, 
          new4(R,B,C,D,S,T,U,H,I,J,K,L,M,N,O,P,Q).
new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- M=< -1, 
          new60(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- M>=1, 
          new60(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- M=0, 
          new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new58(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- 
          new59(A,B,C,D,E,F,G,H,I,J,K,L,R,N,O,P,Q).
new56(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=A+E+F+G, S= -1+B, T=0, U=1, V=0, 
          B>=1, new4(R,S,C,D,T,U,V,H,I,J,K,L,M,N,O,P,Q).
new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- N=< -1, 
          new56(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- N>=1, 
          new56(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- N=0, 
          new58(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new54(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- 
          new55(A,B,C,D,E,F,G,H,I,J,K,L,M,R,O,P,Q).
new52(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R= -1+C, S=1+D, C>=1, 
          new4(A,B,R,S,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new51(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- O=< -1, 
          new52(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new51(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- O>=1, 
          new52(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new51(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- O=0, 
          new54(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new50(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- 
          new51(A,B,C,D,E,F,G,H,I,J,K,L,M,N,R,P,Q).
new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R= -1+F, S=1+G, F>=1, 
          new4(A,B,C,D,E,R,S,H,I,J,K,L,M,N,O,P,Q).
new47(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- P=< -1, 
          new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new47(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- P>=1, 
          new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new47(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- P=0, 
          new50(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new45(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- A=0.
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=1, A+B+C+D+E+F+G>=1, 
          new45(R,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=0, A+B+C+D+E+F+G=<0, 
          new45(R,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- A=0.
new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- A=< -1, 
          new43(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- A>=1, 
          new43(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=1, F>=0, 
          new41(R,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=0, F=< -1, 
          new41(R,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- A=0.
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- A=< -1, 
          new39(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- A>=1, 
          new39(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=1, G>=0, 
          new37(R,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=0, G=< -1, 
          new37(R,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- A=0.
new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- A=< -1, 
          new35(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- A>=1, 
          new35(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=1, E>=0, 
          new33(R,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=0, E=< -1, 
          new33(R,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- A=0.
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- A=< -1, 
          new31(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- A>=1, 
          new31(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=1, C>=0, 
          new29(R,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=0, C=< -1, 
          new29(R,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- A=0.
new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- A=< -1, 
          new27(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- A>=1, 
          new27(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=1, D>=0, 
          new25(R,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=0, D=< -1, 
          new25(R,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- A=0.
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- A=< -1, 
          new23(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- A>=1, 
          new23(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=1, B>=0, 
          new21(R,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=0, B=< -1, 
          new21(R,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- A=0.
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- A=< -1, 
          new19(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- A>=1, 
          new19(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=1, A>=0, 
          new17(R,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :- R=0, A=< -1, 
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
          new47(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,R,Q).
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
