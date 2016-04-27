new50(A,B,C,D,E,F,G,H,I) :- J= -1+B, K= -1+B, L= -2+2*B, C>=2, 
          new9(A,J,C,K,L,F,G,H,I).
new48(A,B,C,D,E,F,G,H,I) :- J=2*B, C>=3, new9(A,B,C,B,J,F,G,H,I).
new47(A,B,C,D,E,F,G,H,I) :- J= -1+C, B=<1, new48(A,B,J,D,E,F,G,H,I).
new45(A,B,C,D,E,F,G,H,I) :- B>=2, new50(A,B,C,D,E,F,G,H,I).
new44(A,B,C,D,E,F,G,H,I) :- F=< -1, new45(A,B,C,D,E,F,G,H,I).
new44(A,B,C,D,E,F,G,H,I) :- F>=1, new45(A,B,C,D,E,F,G,H,I).
new44(A,B,C,D,E,F,G,H,I) :- F=0, new47(A,B,C,D,E,F,G,H,I).
new43(A,B,C,D,E,F,G,H,I) :- new44(A,B,C,D,E,J,G,H,I).
new41(A,B,C,D,E,F,G,H,I) :- J=2*E, C-E>=0, new9(A,B,C,E,J,F,G,H,I).
new40(A,B,C,D,E,F,G,H,I) :- G=< -1, new41(A,B,C,D,E,F,G,H,I).
new40(A,B,C,D,E,F,G,H,I) :- G>=1, new41(A,B,C,D,E,F,G,H,I).
new40(A,B,C,D,E,F,G,H,I) :- G=0, new43(A,B,C,D,E,F,G,H,I).
new39(A,B,C,D,E,F,G,H,I) :- new40(A,B,C,D,E,F,J,H,I).
new37(A,B,C,D,E,F,G,H,I) :- J=1+E, K=2+2*E, C-E>=1, new9(A,B,C,J,K,F,G,H,I).
new36(A,B,C,D,E,F,G,H,I) :- H=< -1, new37(A,B,C,D,E,F,G,H,I).
new36(A,B,C,D,E,F,G,H,I) :- H>=1, new37(A,B,C,D,E,F,G,H,I).
new36(A,B,C,D,E,F,G,H,I) :- H=0, new39(A,B,C,D,E,F,G,H,I).
new34(A,B,C,D,E,F,G,H,I,J) :- A=0.
new32(A,B,C,D,E,F,G,H,I) :- J=1, A-C>=0, new34(J,A,B,C,D,E,F,G,H,I).
new32(A,B,C,D,E,F,G,H,I) :- J=0, A-C=< -1, new34(J,A,B,C,D,E,F,G,H,I).
new30(A,B,C,D,E,F,G,H,I,J) :- A=0.
new30(A,B,C,D,E,F,G,H,I,J) :- A=< -1, new32(B,C,D,E,F,G,H,I,J).
new30(A,B,C,D,E,F,G,H,I,J) :- A>=1, new32(B,C,D,E,F,G,H,I,J).
new28(A,B,C,D,E,F,G,H,I) :- J=1, B>=1, new30(J,A,B,C,D,E,F,G,H,I).
new28(A,B,C,D,E,F,G,H,I) :- J=0, B=<0, new30(J,A,B,C,D,E,F,G,H,I).
new26(A,B,C,D,E,F,G,H,I,J) :- A=0.
new26(A,B,C,D,E,F,G,H,I,J) :- A=< -1, new28(B,C,D,E,F,G,H,I,J).
new26(A,B,C,D,E,F,G,H,I,J) :- A>=1, new28(B,C,D,E,F,G,H,I,J).
new24(A,B,C,D,E,F,G,H,I) :- J=1, C>=2, new26(J,A,B,C,D,E,F,G,H,I).
new24(A,B,C,D,E,F,G,H,I) :- J=0, C=<1, new26(J,A,B,C,D,E,F,G,H,I).
new22(A,B,C,D,E,F,G,H,I,J) :- A=0.
new22(A,B,C,D,E,F,G,H,I,J) :- A=< -1, new24(B,C,D,E,F,G,H,I,J).
new22(A,B,C,D,E,F,G,H,I,J) :- A>=1, new24(B,C,D,E,F,G,H,I,J).
new20(A,B,C,D,E,F,G,H,I) :- J=1, 2*B-1*C=<1, new22(J,A,B,C,D,E,F,G,H,I).
new20(A,B,C,D,E,F,G,H,I) :- J=0, 2*B-1*C>=2, new22(J,A,B,C,D,E,F,G,H,I).
new18(A,B,C,D,E,F,G,H,I,J) :- A=0.
new18(A,B,C,D,E,F,G,H,I,J) :- A=< -1, new20(B,C,D,E,F,G,H,I,J).
new18(A,B,C,D,E,F,G,H,I,J) :- A>=1, new20(B,C,D,E,F,G,H,I,J).
new16(A,B,C,D,E,F,G,H,I) :- J=1, 2*D-1*E=<0, new18(J,A,B,C,D,E,F,G,H,I).
new16(A,B,C,D,E,F,G,H,I) :- J=0, 2*D-1*E>=1, new18(J,A,B,C,D,E,F,G,H,I).
new14(A,B,C,D,E,F,G,H,I,J) :- A=0.
new14(A,B,C,D,E,F,G,H,I,J) :- A=< -1, new16(B,C,D,E,F,G,H,I,J).
new14(A,B,C,D,E,F,G,H,I,J) :- A>=1, new16(B,C,D,E,F,G,H,I,J).
new13(A,B,C,D,E,F,G,H,I) :- J=1, 2*D-1*E>=0, new14(J,A,B,C,D,E,F,G,H,I).
new13(A,B,C,D,E,F,G,H,I) :- J=0, 2*D-1*E=< -1, new14(J,A,B,C,D,E,F,G,H,I).
new11(A,B,C,D,E,F,G,H,I) :- new36(A,B,C,D,E,F,G,J,I).
new10(A,B,C,D,E,F,G,H,I) :- I=< -1, new11(A,B,C,D,E,F,G,H,I).
new10(A,B,C,D,E,F,G,H,I) :- I>=1, new11(A,B,C,D,E,F,G,H,I).
new10(A,B,C,D,E,F,G,H,I) :- I=0, new13(A,B,C,D,E,F,G,H,I).
new9(A,B,C,D,E,F,G,H,I) :- new10(A,B,C,D,E,F,G,H,J).
new8(A,B,C,D,E,F,G,H,I) :- A-2*B>= -1, new9(A,B,C,D,E,F,G,H,I).
new7(A,B,C,D,E,F,G,H,I) :- A-2*B=<0, new8(A,B,C,D,E,F,G,H,I).
new6(A,B,C,D,E,F,G,H,I) :- 2*B=1*E, new7(A,B,C,D,E,F,G,H,I).
new5(A,B,C,B,D,E,F,G,H) :- new6(A,B,C,B,D,E,F,G,H).
new4(A,B,A,C,D,E,F,G,H) :- new5(A,B,A,C,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H,I) :- A>=2, new4(A,B,C,D,E,F,G,H,I).
new2 :- new3(A,B,C,D,E,F,G,H,I).
new1 :- new2.
false :- new1.
