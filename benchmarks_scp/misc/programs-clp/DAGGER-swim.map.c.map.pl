new81(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P= -1+A, Q=1+B, R= -1+G, G>=1, 
          new7(P,Q,C,D,E,F,R,H,I,J,K,L,M,N,O).
new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P= -1+C, Q=1+D, R= -1+F, F>=1, 
          new7(A,B,P,Q,E,R,G,H,I,J,K,L,M,N,O).
new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P= -1+E, Q=1+F, E>=1, 
          new7(A,B,C,D,P,Q,G,H,I,J,K,L,M,N,O).
new73(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P= -1+D, Q=1+E, R=1+G, D>=1, 
          new7(A,B,C,P,Q,F,R,H,I,J,K,L,M,N,O).
new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- J=< -1, 
          new73(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- J>=1, 
          new73(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- J=0, 
          new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new71(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- new72(A,B,C,D,E,F,G,H,I,P,K,L,M,N,O).
new69(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- C>=1, 
          new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new68(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- K=< -1, 
          new69(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new68(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- K>=1, 
          new69(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new68(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- K=0, 
          new71(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new67(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- new68(A,B,C,D,E,F,G,H,I,J,P,L,M,N,O).
new65(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P= -1+B, Q=1+C, R=1+F, B>=1, 
          new7(A,P,Q,D,E,R,G,H,I,J,K,L,M,N,O).
new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- L=< -1, 
          new65(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- L>=1, 
          new65(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- L=0, 
          new67(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- new64(A,B,C,D,E,F,G,H,I,J,K,P,M,N,O).
new61(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- A>=1, 
          new81(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new60(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- M=< -1, 
          new61(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new60(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- M>=1, 
          new61(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new60(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- M=0, 
          new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- new60(A,B,C,D,E,F,G,H,I,J,K,L,P,N,O).
new57(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=1+A, Q= -1+F, F>=1, 
          new7(P,B,C,D,E,Q,G,H,I,J,K,L,M,N,O).
new56(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- N=< -1, 
          new57(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new56(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- N>=1, 
          new57(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new56(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- N=0, 
          new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new54(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=0.
new52(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=1, A+B+D+E+F>=1, 
          new54(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new52(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=0, A+B+D+E+F=<0, 
          new54(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new50(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=0.
new50(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=< -1, 
          new52(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new50(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A>=1, 
          new52(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=1, B+C+D+G>=1, 
          new50(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=0, B+C+D+G=<0, 
          new50(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=0.
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=< -1, 
          new48(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A>=1, 
          new48(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=1, A>=0, 
          new46(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=0, A=< -1, 
          new46(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=0.
new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=< -1, 
          new44(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A>=1, 
          new44(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=1, B>=0, 
          new42(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=0, B=< -1, 
          new42(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=0.
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=< -1, 
          new40(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A>=1, 
          new40(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=1, C>=0, 
          new38(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=0, C=< -1, 
          new38(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=0.
new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=< -1, 
          new36(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A>=1, 
          new36(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=1, D>=0, 
          new34(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=0, D=< -1, 
          new34(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=0.
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=< -1, 
          new32(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A>=1, 
          new32(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=1, E>=0, 
          new30(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=0, E=< -1, 
          new30(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=0.
new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=< -1, 
          new28(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A>=1, 
          new28(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=1, F>=0, 
          new26(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=0, F=< -1, 
          new26(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=0.
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=< -1, 
          new24(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A>=1, 
          new24(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=1, G>=0, 
          new22(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=0, G=< -1, 
          new22(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=0.
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A=< -1, 
          new20(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- A>=1, 
          new20(B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=1, A = -(B)-D-E-F+H,
          new17(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=0, A+B+D+E+F-H=< -1, 
          new17(P,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- P=0, A+B+D+E+F-H>=1, 
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
new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :- new56(A,B,C,D,E,F,G,H,I,J,K,L,M,P,O).
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
