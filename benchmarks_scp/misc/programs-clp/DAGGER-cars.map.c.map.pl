new53(A,B,C,D,E,F,G,H,I) :- J=A+B, K=C+D, L=1+D, M=E+F, N=1+G, A-2*C+E>=0, 
          new9(J,B,K,L,M,F,N,H,I).
new51(A,B,C,D,E,F,G,H,I) :- J=A+B, K=C+D, L= -1+D, M=E+F, N=1+G, A-2*C+E=<0, 
          new9(J,B,K,L,M,F,N,H,I).
new50(A,B,C,D,E,F,G,H,I) :- H=< -1, new51(A,B,C,D,E,F,G,H,I).
new50(A,B,C,D,E,F,G,H,I) :- H>=1, new51(A,B,C,D,E,F,G,H,I).
new50(A,B,C,D,E,F,G,H,I) :- H=0, new53(A,B,C,D,E,F,G,H,I).
new49(A,B,C,D,E,F,G,H,I) :- new50(A,B,C,D,E,F,G,J,I).
new48(A,B,C,D,E,F,G,H,I) :- D=<5, new49(A,B,C,D,E,F,G,H,I).
new46(A,B,C,D,E,F,G,H,I,J) :- A=0.
new44(A,B,C,D,E,F,G,H,I) :- J=1, B-F>=0, new46(J,A,B,C,D,E,F,G,H,I).
new44(A,B,C,D,E,F,G,H,I) :- J=0, B-F=< -1, new46(J,A,B,C,D,E,F,G,H,I).
new42(A,B,C,D,E,F,G,H,I,J) :- A=0.
new42(A,B,C,D,E,F,G,H,I,J) :- A=< -1, new44(B,C,D,E,F,G,H,I,J).
new42(A,B,C,D,E,F,G,H,I,J) :- A>=1, new44(B,C,D,E,F,G,H,I,J).
new40(A,B,C,D,E,F,G,H,I) :- J=1, B-2*D+F+2*G>=0, new42(J,A,B,C,D,E,F,G,H,I).
new40(A,B,C,D,E,F,G,H,I) :- J=0, B-2*D+F+2*G=< -1, new42(J,A,B,C,D,E,F,G,H,I).
new38(A,B,C,D,E,F,G,H,I,J) :- A=0.
new38(A,B,C,D,E,F,G,H,I,J) :- A=< -1, new40(B,C,D,E,F,G,H,I,J).
new38(A,B,C,D,E,F,G,H,I,J) :- A>=1, new40(B,C,D,E,F,G,H,I,J).
new36(A,B,C,D,E,F,G,H,I) :- J=1, C+5*G>=75, new38(J,A,B,C,D,E,F,G,H,I).
new36(A,B,C,D,E,F,G,H,I) :- J=0, C+5*G=<74, new38(J,A,B,C,D,E,F,G,H,I).
new34(A,B,C,D,E,F,G,H,I,J) :- A=0.
new34(A,B,C,D,E,F,G,H,I,J) :- A=< -1, new36(B,C,D,E,F,G,H,I,J).
new34(A,B,C,D,E,F,G,H,I,J) :- A>=1, new36(B,C,D,E,F,G,H,I,J).
new32(A,B,C,D,E,F,G,H,I) :- J=1, D>= -6, new34(J,A,B,C,D,E,F,G,H,I).
new32(A,B,C,D,E,F,G,H,I) :- J=0, D=< -7, new34(J,A,B,C,D,E,F,G,H,I).
new30(A,B,C,D,E,F,G,H,I,J) :- A=0.
new30(A,B,C,D,E,F,G,H,I,J) :- A=< -1, new32(B,C,D,E,F,G,H,I,J).
new30(A,B,C,D,E,F,G,H,I,J) :- A>=1, new32(B,C,D,E,F,G,H,I,J).
new28(A,B,C,D,E,F,G,H,I) :- J=1, F>=0, new30(J,A,B,C,D,E,F,G,H,I).
new28(A,B,C,D,E,F,G,H,I) :- J=0, F=< -1, new30(J,A,B,C,D,E,F,G,H,I).
new26(A,B,C,D,E,F,G,H,I,J) :- A=0.
new26(A,B,C,D,E,F,G,H,I,J) :- A=< -1, new28(B,C,D,E,F,G,H,I,J).
new26(A,B,C,D,E,F,G,H,I,J) :- A>=1, new28(B,C,D,E,F,G,H,I,J).
new24(A,B,C,D,E,F,G,H,I) :- J=1, D=<6, new26(J,A,B,C,D,E,F,G,H,I).
new24(A,B,C,D,E,F,G,H,I) :- J=0, D>=7, new26(J,A,B,C,D,E,F,G,H,I).
new22(A,B,C,D,E,F,G,H,I,J) :- A=0.
new22(A,B,C,D,E,F,G,H,I,J) :- A=< -1, new24(B,C,D,E,F,G,H,I,J).
new22(A,B,C,D,E,F,G,H,I,J) :- A>=1, new24(B,C,D,E,F,G,H,I,J).
new20(A,B,C,D,E,F,G,H,I) :- J=1, C-5*G=<75, new22(J,A,B,C,D,E,F,G,H,I).
new20(A,B,C,D,E,F,G,H,I) :- J=0, C-5*G>=76, new22(J,A,B,C,D,E,F,G,H,I).
new18(A,B,C,D,E,F,G,H,I,J) :- A=0.
new18(A,B,C,D,E,F,G,H,I,J) :- A=< -1, new20(B,C,D,E,F,G,H,I,J).
new18(A,B,C,D,E,F,G,H,I,J) :- A>=1, new20(B,C,D,E,F,G,H,I,J).
new16(A,B,C,D,E,F,G,H,I) :- J=1, B-2*D+F-2*G=<0, new18(J,A,B,C,D,E,F,G,H,I).
new16(A,B,C,D,E,F,G,H,I) :- J=0, B-2*D+F-2*G>=1, new18(J,A,B,C,D,E,F,G,H,I).
new14(A,B,C,D,E,F,G,H,I,J) :- A=0.
new14(A,B,C,D,E,F,G,H,I,J) :- A=< -1, new16(B,C,D,E,F,G,H,I,J).
new14(A,B,C,D,E,F,G,H,I,J) :- A>=1, new16(B,C,D,E,F,G,H,I,J).
new13(A,B,C,D,E,F,G,H,I) :- J=1, B=<5, new14(J,A,B,C,D,E,F,G,H,I).
new13(A,B,C,D,E,F,G,H,I) :- J=0, B>=6, new14(J,A,B,C,D,E,F,G,H,I).
new11(A,B,C,D,E,F,G,H,I) :- D>= -5, new48(A,B,C,D,E,F,G,H,I).
new10(A,B,C,D,E,F,G,H,I) :- I=< -1, new11(A,B,C,D,E,F,G,H,I).
new10(A,B,C,D,E,F,G,H,I) :- I>=1, new11(A,B,C,D,E,F,G,H,I).
new10(A,B,C,D,E,F,G,H,I) :- I=0, new13(A,B,C,D,E,F,G,H,I).
new9(A,B,C,D,E,F,G,H,I) :- new10(A,B,C,D,E,F,G,H,J).
new8(A,B,C,D,E,F,G,H,I) :- D=<5, new9(A,B,C,D,E,F,G,H,I).
new7(A,B,C,D,E,F,G,H,I) :- D>= -5, new8(A,B,C,D,E,F,G,H,I).
new6(A,B,C,D,E,F,G,H,I) :- J=0, B=2*D-F, new7(A,B,C,D,E,F,J,H,I).
new5(A,B,C,D,E,F,G,H,I) :- B-F>=0, new6(A,B,C,D,E,F,G,H,I).
new4(A,B,C,D,E,F,G,H,I) :- B=<5, new5(A,B,C,D,E,F,G,H,I).
new3(A,B,C,D,E,F,G,H,I) :- F>=0, new4(A,B,C,D,E,F,G,H,I).
new2 :- A= -50, B=100, C=75, new3(B,D,C,E,A,F,G,H,I).
new1 :- new2.
false :- new1.
