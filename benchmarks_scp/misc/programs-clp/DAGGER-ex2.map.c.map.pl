new48(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A=0.
new45(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1, A=<132, new48(M,A,B,C,D,E,F,G,H,I,J,K,L).
new45(A,B,C,D,E,F,G,H,I,J,K,L) :- M=0, A>=133, new48(M,A,B,C,D,E,F,G,H,I,J,K,L).
new44(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1+A, L=< -1, new45(M,B,C,D,E,F,G,H,I,J,K,L).
new44(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1+A, L>=1, new45(M,B,C,D,E,F,G,H,I,J,K,L).
new44(A,B,C,D,E,F,G,H,I,J,K,L) :- M=2+A, L=0, new45(M,B,C,D,E,F,G,H,I,J,K,L).
new41(A,B,C,D,E,F,G,H,I,J,K,L) :- new44(A,B,C,D,E,F,G,H,I,J,K,M).
new40(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1+A, K=< -1, new41(M,B,C,D,E,F,G,H,I,J,K,L).
new40(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1+A, K>=1, new41(M,B,C,D,E,F,G,H,I,J,K,L).
new40(A,B,C,D,E,F,G,H,I,J,K,L) :- M=4+A, K=0, new41(M,B,C,D,E,F,G,H,I,J,K,L).
new37(A,B,C,D,E,F,G,H,I,J,K,L) :- new40(A,B,C,D,E,F,G,H,I,J,M,L).
new36(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1+A, J=< -1, new37(M,B,C,D,E,F,G,H,I,J,K,L).
new36(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1+A, J>=1, new37(M,B,C,D,E,F,G,H,I,J,K,L).
new36(A,B,C,D,E,F,G,H,I,J,K,L) :- M=6+A, J=0, new37(M,B,C,D,E,F,G,H,I,J,K,L).
new33(A,B,C,D,E,F,G,H,I,J,K,L) :- new36(A,B,C,D,E,F,G,H,I,M,K,L).
new32(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1+A, I=< -1, new33(M,B,C,D,E,F,G,H,I,J,K,L).
new32(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1+A, I>=1, new33(M,B,C,D,E,F,G,H,I,J,K,L).
new32(A,B,C,D,E,F,G,H,I,J,K,L) :- M=8+A, I=0, new33(M,B,C,D,E,F,G,H,I,J,K,L).
new29(A,B,C,D,E,F,G,H,I,J,K,L) :- new32(A,B,C,D,E,F,G,H,M,J,K,L).
new28(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1+A, H=< -1, new29(M,B,C,D,E,F,G,H,I,J,K,L).
new28(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1+A, H>=1, new29(M,B,C,D,E,F,G,H,I,J,K,L).
new28(A,B,C,D,E,F,G,H,I,J,K,L) :- M=10+A, H=0, new29(M,B,C,D,E,F,G,H,I,J,K,L).
new25(A,B,C,D,E,F,G,H,I,J,K,L) :- new28(A,B,C,D,E,F,G,M,I,J,K,L).
new24(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1+A, G=< -1, new25(M,B,C,D,E,F,G,H,I,J,K,L).
new24(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1+A, G>=1, new25(M,B,C,D,E,F,G,H,I,J,K,L).
new24(A,B,C,D,E,F,G,H,I,J,K,L) :- M=12+A, G=0, new25(M,B,C,D,E,F,G,H,I,J,K,L).
new21(A,B,C,D,E,F,G,H,I,J,K,L) :- new24(A,B,C,D,E,F,M,H,I,J,K,L).
new20(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1+A, F=< -1, new21(M,B,C,D,E,F,G,H,I,J,K,L).
new20(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1+A, F>=1, new21(M,B,C,D,E,F,G,H,I,J,K,L).
new20(A,B,C,D,E,F,G,H,I,J,K,L) :- M=14+A, F=0, new21(M,B,C,D,E,F,G,H,I,J,K,L).
new17(A,B,C,D,E,F,G,H,I,J,K,L) :- new20(A,B,C,D,E,M,G,H,I,J,K,L).
new16(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1+A, E=< -1, new17(M,B,C,D,E,F,G,H,I,J,K,L).
new16(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1+A, E>=1, new17(M,B,C,D,E,F,G,H,I,J,K,L).
new16(A,B,C,D,E,F,G,H,I,J,K,L) :- M=16+A, E=0, new17(M,B,C,D,E,F,G,H,I,J,K,L).
new13(A,B,C,D,E,F,G,H,I,J,K,L) :- new16(A,B,C,D,M,F,G,H,I,J,K,L).
new12(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1+A, D=< -1, new13(M,B,C,D,E,F,G,H,I,J,K,L).
new12(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1+A, D>=1, new13(M,B,C,D,E,F,G,H,I,J,K,L).
new12(A,B,C,D,E,F,G,H,I,J,K,L) :- M=18+A, D=0, new13(M,B,C,D,E,F,G,H,I,J,K,L).
new9(A,B,C,D,E,F,G,H,I,J,K,L) :- new12(A,B,C,M,E,F,G,H,I,J,K,L).
new8(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1+A, C=< -1, new9(M,B,C,D,E,F,G,H,I,J,K,L).
new8(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1+A, C>=1, new9(M,B,C,D,E,F,G,H,I,J,K,L).
new8(A,B,C,D,E,F,G,H,I,J,K,L) :- M=20+A, C=0, new9(M,B,C,D,E,F,G,H,I,J,K,L).
new5(A,B,C,D,E,F,G,H,I,J,K,L) :- new8(A,B,M,D,E,F,G,H,I,J,K,L).
new4(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1+A, B=< -1, new5(M,B,C,D,E,F,G,H,I,J,K,L).
new4(A,B,C,D,E,F,G,H,I,J,K,L) :- M=1+A, B>=1, new5(M,B,C,D,E,F,G,H,I,J,K,L).
new4(A,B,C,D,E,F,G,H,I,J,K,L) :- M=22+A, B=0, new5(M,B,C,D,E,F,G,H,I,J,K,L).
new3(A,B,C,D,E,F,G,H,I,J,K,L) :- new4(A,M,C,D,E,F,G,H,I,J,K,L).
new2 :- A=0, new3(A,B,C,D,E,F,G,H,I,J,K,L).
new1 :- new2.
false :- new1.
