new92(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P= -1+A, Q=1+B, R= -1+G, G>=1, 
          new7(P,Q,C,D,E,F,R,H,I,J,K,L,M,N,O).
new89(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P= -1+C, Q=1+D, R= -1+F, F>=1, 
          new7(A,B,P,Q,E,R,G,H,I,J,K,L,M,N,O).
new86(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P= -1+E, Q=1+F, E>=1, 
          new7(A,B,C,D,P,Q,G,H,I,J,K,L,M,N,O).
new84(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P= -1+D, Q=1+E, R=1+G, D>=1, 
          new7(A,B,C,P,Q,F,R,H,I,J,K,L,M,N,O).
new83(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- J=< -1, 
          new84(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new83(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- J>=1, 
          new84(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new83(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- J=0, 
          new86(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new82(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- new83(A,B,C,D,E,F,G,H,I,P,K,L,M,N,O).
new80(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- C>=1, 
          new89(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new79(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- K=< -1, 
          new80(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new79(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- K>=1, 
          new80(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new79(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- K=0, 
          new82(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- new79(A,B,C,D,E,F,G,H,I,J,P,L,M,N,O).
new76(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P= -1+B, Q=1+C, R=1+F, B>=1, 
          new7(A,P,Q,D,E,R,G,H,I,J,K,L,M,N,O).
new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- L=< -1, 
          new76(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- L>=1, 
          new76(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- L=0, 
          new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new74(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- new75(A,B,C,D,E,F,G,H,I,J,K,P,M,N,O).
new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- A>=1, 
          new92(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new71(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- M=< -1, 
          new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new71(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- M>=1, 
          new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new71(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- M=0, 
          new74(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new70(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- new71(A,B,C,D,E,F,G,H,I,J,K,L,P,N,O).
new68(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=1+A, Q= -1+F, F>=1, 
          new7(P,B,C,D,E,Q,G,H,I,J,K,L,M,N,O).
new67(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- N=< -1, 
          new68(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new67(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- N>=1, 
          new68(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new67(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- N=0, 
          new70(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new65(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=0.
new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=1, A+B+D+F+G>=1, 
          new65(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=0, A+B+D+F+G=<0, 
          new65(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new61(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=0.
new61(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=< -1, 
          new63(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new61(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A>=1, 
          new63(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=1, A+B+D+E+F>=1, 
          new61(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=0, A+B+D+E+F=<0, 
          new61(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new57(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=0.
new57(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=< -1, 
          new59(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new57(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A>=1, 
          new59(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=1, B+C+D+G>=1, 
          new57(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=0, B+C+D+G=<0, 
          new57(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new53(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=0.
new53(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=< -1, 
          new55(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new53(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A>=1, 
          new55(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new51(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=1, A>=0, 
          new53(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new51(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=0, A=< -1, 
          new53(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=0.
new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=< -1, 
          new51(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A>=1, 
          new51(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new47(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=1, B>=0, 
          new49(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new47(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=0, B=< -1, 
          new49(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new45(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=0.
new45(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=< -1, 
          new47(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new45(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A>=1, 
          new47(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=1, C>=0, 
          new45(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=0, C=< -1, 
          new45(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=0.
new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=< -1, 
          new43(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A>=1, 
          new43(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=1, D>=0, 
          new41(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=0, D=< -1, 
          new41(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=0.
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=< -1, 
          new39(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A>=1, 
          new39(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=1, E>=0, 
          new37(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=0, E=< -1, 
          new37(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=0.
new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=< -1, 
          new35(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A>=1, 
          new35(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=1, F>=0, 
          new33(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=0, F=< -1, 
          new33(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=0.
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=< -1, 
          new31(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A>=1, 
          new31(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=1, G>=0, 
          new29(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=0, G=< -1, 
          new29(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=0.
new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=< -1, 
          new27(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A>=1, 
          new27(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=1, A+B+D+E+F-H=<0, 
          new25(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=0, A+B+D+E+F-H>=1, 
          new25(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=0.
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=< -1, 
          new23(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A>=1, 
          new23(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=1, A+B+D+E+F-H>=0, 
          new21(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=0, A+B+D+E+F-H=< -1, 
          new21(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=0.
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=< -1, 
          new19(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A>=1, 
          new19(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=1, B+C+D+G-I>=0, 
          new17(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=0, B+C+D+G-I=< -1, 
          new17(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=0.
new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=< -1, 
          new15(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A>=1, 
          new15(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=1, B = -(C)-D-G+I, 
          new12(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=0, B+C+D+G-I=< -1, 
          new12(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=0, B+C+D+G-I>=1, 
          new12(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- new67(A,B,C,D,E,F,G,H,I,J,K,L,M,P,O).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- O=< -1, 
          new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- O>=1, 
          new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- O=0, 
          new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,P).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- I>=1, 
          new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- H>=1, 
          new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new4(A,B,C,D,E,F,G,H,G,I,J,K,L,M,N) :- new5(A,B,C,D,E,F,G,H,G,I,J,K,L,M,N).
new3(A,B,C,D,E,F,G,F,H,I,J,K,L,M,N) :- new4(A,B,C,D,E,F,G,F,H,I,J,K,L,M,N).
new2 :- A=0, B=0, C=0, D=0, E=0, new3(B,C,D,E,A,F,G,H,I,J,K,L,M,N,O).
new1 :- new2.
false :- new1.
