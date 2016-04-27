new74(A,B,C,D,E,F,G,H,I,J,K) :- L= -1+A, M=1+B, N= -1+D, O=1+E, D>=1, 
          new9(L,M,C,N,O,F,G,H,I,J,K).
new72(A,B,C,D,E,F,G,H,I,J,K) :- L= -1+B, M=1+C, F>=1, 
          new9(A,L,M,D,E,F,G,H,I,J,K).
new70(A,B,C,D,E,F,G,H,I,J,K) :- L=1+B, M= -1+C, C>=1, 
          new9(A,L,M,D,E,F,G,H,I,J,K).
new67(A,B,C,D,E,F,G,H,I,J,K) :- L=1+A, M= -1+B, N=D+F, O=0, B>=1, 
          new9(L,M,C,N,E,O,G,H,I,J,K).
new65(A,B,C,D,E,F,G,H,I,J,K) :- L=1+A, M= -1+C, N=0, O=E+F, C>=1, 
          new9(L,B,M,D,N,O,G,H,I,J,K).
new64(A,B,C,D,E,F,G,H,I,J,K) :- G=< -1, new65(A,B,C,D,E,F,G,H,I,J,K).
new64(A,B,C,D,E,F,G,H,I,J,K) :- G>=1, new65(A,B,C,D,E,F,G,H,I,J,K).
new64(A,B,C,D,E,F,G,H,I,J,K) :- G=0, new67(A,B,C,D,E,F,G,H,I,J,K).
new63(A,B,C,D,E,F,G,H,I,J,K) :- new64(A,B,C,D,E,F,L,H,I,J,K).
new61(A,B,C,D,E,F,G,H,I,J,K) :- D>=1, new70(A,B,C,D,E,F,G,H,I,J,K).
new60(A,B,C,D,E,F,G,H,I,J,K) :- H=< -1, new61(A,B,C,D,E,F,G,H,I,J,K).
new60(A,B,C,D,E,F,G,H,I,J,K) :- H>=1, new61(A,B,C,D,E,F,G,H,I,J,K).
new60(A,B,C,D,E,F,G,H,I,J,K) :- H=0, new63(A,B,C,D,E,F,G,H,I,J,K).
new59(A,B,C,D,E,F,G,H,I,J,K) :- new60(A,B,C,D,E,F,G,L,I,J,K).
new57(A,B,C,D,E,F,G,H,I,J,K) :- B>=1, new72(A,B,C,D,E,F,G,H,I,J,K).
new56(A,B,C,D,E,F,G,H,I,J,K) :- I=< -1, new57(A,B,C,D,E,F,G,H,I,J,K).
new56(A,B,C,D,E,F,G,H,I,J,K) :- I>=1, new57(A,B,C,D,E,F,G,H,I,J,K).
new56(A,B,C,D,E,F,G,H,I,J,K) :- I=0, new59(A,B,C,D,E,F,G,H,I,J,K).
new55(A,B,C,D,E,F,G,H,I,J,K) :- new56(A,B,C,D,E,F,G,H,L,J,K).
new53(A,B,C,D,E,F,G,H,I,J,K) :- A>=1, new74(A,B,C,D,E,F,G,H,I,J,K).
new52(A,B,C,D,E,F,G,H,I,J,K) :- J=< -1, new53(A,B,C,D,E,F,G,H,I,J,K).
new52(A,B,C,D,E,F,G,H,I,J,K) :- J>=1, new53(A,B,C,D,E,F,G,H,I,J,K).
new52(A,B,C,D,E,F,G,H,I,J,K) :- J=0, new55(A,B,C,D,E,F,G,H,I,J,K).
new50(A,B,C,D,E,F,G,H,I,J,K,L) :- A=0.
new48(A,B,C,D,E,F,G,H,I,J,K) :- L=1, A+B+C>=1, new50(L,A,B,C,D,E,F,G,H,I,J,K).
new48(A,B,C,D,E,F,G,H,I,J,K) :- L=0, A+B+C=<0, new50(L,A,B,C,D,E,F,G,H,I,J,K).
new46(A,B,C,D,E,F,G,H,I,J,K,L) :- A=0.
new46(A,B,C,D,E,F,G,H,I,J,K,L) :- A=< -1, new48(B,C,D,E,F,G,H,I,J,K,L).
new46(A,B,C,D,E,F,G,H,I,J,K,L) :- A>=1, new48(B,C,D,E,F,G,H,I,J,K,L).
new44(A,B,C,D,E,F,G,H,I,J,K) :- L=1, A+B-D-E>=0, new46(L,A,B,C,D,E,F,G,H,I,J,K).
new44(A,B,C,D,E,F,G,H,I,J,K) :- L=0, A+B-D-E=< -1, 
          new46(L,A,B,C,D,E,F,G,H,I,J,K).
new42(A,B,C,D,E,F,G,H,I,J,K,L) :- A=0.
new42(A,B,C,D,E,F,G,H,I,J,K,L) :- A=< -1, new44(B,C,D,E,F,G,H,I,J,K,L).
new42(A,B,C,D,E,F,G,H,I,J,K,L) :- A>=1, new44(B,C,D,E,F,G,H,I,J,K,L).
new40(A,B,C,D,E,F,G,H,I,J,K) :- L=1, A+E>=1, new42(L,A,B,C,D,E,F,G,H,I,J,K).
new40(A,B,C,D,E,F,G,H,I,J,K) :- L=0, A+E=<0, new42(L,A,B,C,D,E,F,G,H,I,J,K).
new38(A,B,C,D,E,F,G,H,I,J,K,L) :- A=0.
new38(A,B,C,D,E,F,G,H,I,J,K,L) :- A=< -1, new40(B,C,D,E,F,G,H,I,J,K,L).
new38(A,B,C,D,E,F,G,H,I,J,K,L) :- A>=1, new40(B,C,D,E,F,G,H,I,J,K,L).
new36(A,B,C,D,E,F,G,H,I,J,K) :- L=1, B>=0, new38(L,A,B,C,D,E,F,G,H,I,J,K).
new36(A,B,C,D,E,F,G,H,I,J,K) :- L=0, B=< -1, new38(L,A,B,C,D,E,F,G,H,I,J,K).
new34(A,B,C,D,E,F,G,H,I,J,K,L) :- A=0.
new34(A,B,C,D,E,F,G,H,I,J,K,L) :- A=< -1, new36(B,C,D,E,F,G,H,I,J,K,L).
new34(A,B,C,D,E,F,G,H,I,J,K,L) :- A>=1, new36(B,C,D,E,F,G,H,I,J,K,L).
new32(A,B,C,D,E,F,G,H,I,J,K) :- L=1, C>=0, new34(L,A,B,C,D,E,F,G,H,I,J,K).
new32(A,B,C,D,E,F,G,H,I,J,K) :- L=0, C=< -1, new34(L,A,B,C,D,E,F,G,H,I,J,K).
new30(A,B,C,D,E,F,G,H,I,J,K,L) :- A=0.
new30(A,B,C,D,E,F,G,H,I,J,K,L) :- A=< -1, new32(B,C,D,E,F,G,H,I,J,K,L).
new30(A,B,C,D,E,F,G,H,I,J,K,L) :- A>=1, new32(B,C,D,E,F,G,H,I,J,K,L).
new28(A,B,C,D,E,F,G,H,I,J,K) :- L=1, D>=0, new30(L,A,B,C,D,E,F,G,H,I,J,K).
new28(A,B,C,D,E,F,G,H,I,J,K) :- L=0, D=< -1, new30(L,A,B,C,D,E,F,G,H,I,J,K).
new26(A,B,C,D,E,F,G,H,I,J,K,L) :- A=0.
new26(A,B,C,D,E,F,G,H,I,J,K,L) :- A=< -1, new28(B,C,D,E,F,G,H,I,J,K,L).
new26(A,B,C,D,E,F,G,H,I,J,K,L) :- A>=1, new28(B,C,D,E,F,G,H,I,J,K,L).
new24(A,B,C,D,E,F,G,H,I,J,K) :- L=1, E>=0, new26(L,A,B,C,D,E,F,G,H,I,J,K).
new24(A,B,C,D,E,F,G,H,I,J,K) :- L=0, E=< -1, new26(L,A,B,C,D,E,F,G,H,I,J,K).
new22(A,B,C,D,E,F,G,H,I,J,K,L) :- A=0.
new22(A,B,C,D,E,F,G,H,I,J,K,L) :- A=< -1, new24(B,C,D,E,F,G,H,I,J,K,L).
new22(A,B,C,D,E,F,G,H,I,J,K,L) :- A>=1, new24(B,C,D,E,F,G,H,I,J,K,L).
new20(A,B,C,D,E,F,G,H,I,J,K) :- L=1, D+E=<1, new22(L,A,B,C,D,E,F,G,H,I,J,K).
new20(A,B,C,D,E,F,G,H,I,J,K) :- L=0, D+E>=2, new22(L,A,B,C,D,E,F,G,H,I,J,K).
new18(A,B,C,D,E,F,G,H,I,J,K,L) :- A=0.
new18(A,B,C,D,E,F,G,H,I,J,K,L) :- A=< -1, new20(B,C,D,E,F,G,H,I,J,K,L).
new18(A,B,C,D,E,F,G,H,I,J,K,L) :- A>=1, new20(B,C,D,E,F,G,H,I,J,K,L).
new16(A,B,C,D,E,F,G,H,I,J,K) :- L=1, D+E+F=<1, new18(L,A,B,C,D,E,F,G,H,I,J,K).
new16(A,B,C,D,E,F,G,H,I,J,K) :- L=0, D+E+F>=2, new18(L,A,B,C,D,E,F,G,H,I,J,K).
new14(A,B,C,D,E,F,G,H,I,J,K,L) :- A=0.
new14(A,B,C,D,E,F,G,H,I,J,K,L) :- A=< -1, new16(B,C,D,E,F,G,H,I,J,K,L).
new14(A,B,C,D,E,F,G,H,I,J,K,L) :- A>=1, new16(B,C,D,E,F,G,H,I,J,K,L).
new13(A,B,C,D,E,F,G,H,I,J,K) :- L=1, D+E+F>=1, new14(L,A,B,C,D,E,F,G,H,I,J,K).
new13(A,B,C,D,E,F,G,H,I,J,K) :- L=0, D+E+F=<0, new14(L,A,B,C,D,E,F,G,H,I,J,K).
new11(A,B,C,D,E,F,G,H,I,J,K) :- new52(A,B,C,D,E,F,G,H,I,L,K).
new10(A,B,C,D,E,F,G,H,I,J,K) :- K=< -1, new11(A,B,C,D,E,F,G,H,I,J,K).
new10(A,B,C,D,E,F,G,H,I,J,K) :- K>=1, new11(A,B,C,D,E,F,G,H,I,J,K).
new10(A,B,C,D,E,F,G,H,I,J,K) :- K=0, new13(A,B,C,D,E,F,G,H,I,J,K).
new9(A,B,C,D,E,F,G,H,I,J,K) :- new10(A,B,C,D,E,F,G,H,I,J,L).
new8(A,B,C,D,E,F,G,H,I,J,K) :- F=0, new9(A,B,C,D,E,F,G,H,I,J,K).
new7(A,B,C,D,E,F,G,H,I,J,K) :- E=0, new8(A,B,C,D,E,F,G,H,I,J,K).
new6(A,B,C,D,E,F,G,H,I,J,K) :- D=1, new7(A,B,C,D,E,F,G,H,I,J,K).
new5(A,B,C,D,E,F,G,H,I,J,K) :- C=0, new6(A,B,C,D,E,F,G,H,I,J,K).
new4(A,B,C,D,E,F,G,H,I,J,K) :- B=0, new5(A,B,C,D,E,F,G,H,I,J,K).
new3(A,B,C,D,E,F,G,H,I,J,K) :- A>=1, new4(A,B,C,D,E,F,G,H,I,J,K).
new2 :- new3(A,B,C,D,E,F,G,H,I,J,K).
new1 :- new2.
false :- new1.
