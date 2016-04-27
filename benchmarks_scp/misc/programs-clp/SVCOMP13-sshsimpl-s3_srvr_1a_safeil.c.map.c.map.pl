new94(A,B,C,D,E,F,G,H,I,J,K) :- new7(A,B,C,D,E,F,G,H,I,J,K).
new90(A,B,C,D,E,F,G,H,I,J,K) :- new7(A,B,C,D,E,F,G,H,I,J,K).
new87(A,B,C,D,E,F,G,H,I,J,K) :- L=8656, B=< -1, new90(L,B,C,D,E,F,G,H,I,J,K).
new87(A,B,C,D,E,F,G,H,I,J,K) :- L=8656, B>=1, new90(L,B,C,D,E,F,G,H,I,J,K).
new87(A,B,C,D,E,F,G,H,I,J,K) :- L=8512, B=0, new90(L,B,C,D,E,F,G,H,I,J,K).
new78(A,B,C,D,E,F,G,H,I,J,K) :- L=8576, D=< -3, new73(L,B,C,D,E,F,G,H,I,J,K).
new78(A,B,C,D,E,F,G,H,I,J,K) :- L=8576, D>= -1, new73(L,B,C,D,E,F,G,H,I,J,K).
new78(A,B,C,D,E,F,G,H,I,J,K) :- L=8560, D= -2, new73(L,B,C,D,E,F,G,H,I,J,K).
new77(A,B,C,D,E,F,G,H,I,J,K) :- H>=0, new78(A,B,C,D,E,F,G,H,I,J,K).
new75(A,B,C,D,E,F,G,H,I,J,K) :- L=8560, D=< -5, new73(L,B,C,D,E,F,G,H,I,J,K).
new75(A,B,C,D,E,F,G,H,I,J,K) :- L=8560, D>= -3, new73(L,B,C,D,E,F,G,H,I,J,K).
new75(A,B,C,D,E,F,G,H,I,J,K) :- D= -4, new77(A,B,C,D,E,F,G,H,I,J,K).
new73(A,B,C,D,E,F,G,H,I,J,K) :- new7(A,B,C,D,E,F,G,H,I,J,K).
new71(A,B,C,D,E,F,G,H,I,J,K) :- F=< -1, new75(A,B,C,D,E,F,G,H,I,J,K).
new71(A,B,C,D,E,F,G,H,I,J,K) :- F>=1, new75(A,B,C,D,E,F,G,H,I,J,K).
new71(A,B,C,D,E,F,G,H,I,J,K) :- F=0, new77(A,B,C,D,E,F,G,H,I,J,K).
new66(A,B,C,D,E,F,G,H,I,J,K) :- new7(A,B,C,D,E,F,G,H,I,J,K).
new65(A,B,C,D,E,F,G,H,I,J,K) :- L=8466, K=2, new66(L,B,C,D,E,F,G,H,I,J,K).
new65(A,B,C,D,E,F,G,H,I,J,K) :- L=8592, K=<1, new66(L,B,C,D,E,F,G,H,I,J,K).
new65(A,B,C,D,E,F,G,H,I,J,K) :- L=8592, K>=3, new66(L,B,C,D,E,F,G,H,I,J,K).
new61(A,B,C,D,E,F,G,H,I,J,K) :- new7(A,B,C,D,E,F,G,H,I,J,K).
new58(A,B,C,D,E,F,G,H,I,J,K) :- L=8656, B=0, new61(L,B,C,D,E,F,G,H,I,J,K).
new54(A,B,C,D,E,F,G,H,I,J,K) :- new7(A,B,C,D,E,F,G,H,I,J,K).
new51(A,B,C,D,E,F,G,H,I,J,K) :- new7(A,B,C,D,E,F,G,H,I,J,K).
new47(A,B,C,D,E,F,G,H,I,J,K) :- J=5.
new47(A,B,C,D,E,F,G,H,I,J,K) :- J=<4, new46(A,B,C,D,E,F,G,H,I,J,K).
new47(A,B,C,D,E,F,G,H,I,J,K) :- J>=6, new46(A,B,C,D,E,F,G,H,I,J,K).
new46(A,B,C,D,E,F,G,H,I,J,K) :- L=8640, B=< -1, new51(L,B,C,D,E,F,G,H,I,J,K).
new46(A,B,C,D,E,F,G,H,I,J,K) :- L=8640, B>=1, new51(L,B,C,D,E,F,G,H,I,J,K).
new45(A,B,C,D,E,F,G,H,I,J,K) :- L=5, J=4, new46(A,B,C,D,E,F,G,H,I,L,K).
new45(A,B,C,D,E,F,G,H,I,J,K) :- J=<3, new47(A,B,C,D,E,F,G,H,I,J,K).
new45(A,B,C,D,E,F,G,H,I,J,K) :- J>=5, new47(A,B,C,D,E,F,G,H,I,J,K).
new43(A,B,C,D,E,F,G,H,I,J,K) :- A=8672, new45(A,B,C,D,E,F,G,H,I,J,K).
new42(A,B,C,D,E,F,G,H,I,J,K) :- L=8672, M=3, J=2, new54(L,B,C,D,E,F,G,H,I,M,K).
new42(A,B,C,D,E,F,G,H,I,J,K) :- L=8672, J=<1, new54(L,B,C,D,E,F,G,H,I,J,K).
new42(A,B,C,D,E,F,G,H,I,J,K) :- L=8672, J>=3, new54(L,B,C,D,E,F,G,H,I,J,K).
new40(A,B,C,D,E,F,G,H,I,J,K) :- A=8656, new42(A,B,C,D,E,F,G,H,I,J,K).
new40(A,B,C,D,E,F,G,H,I,J,K) :- A=<8655, new43(A,B,C,D,E,F,G,H,I,J,K).
new40(A,B,C,D,E,F,G,H,I,J,K) :- A>=8657, new43(A,B,C,D,E,F,G,H,I,J,K).
new39(A,B,C,D,E,F,G,H,I,J,K) :- L=4, J=3, new58(A,B,C,D,E,F,G,H,I,L,K).
new39(A,B,C,D,E,F,G,H,I,J,K) :- J=<2, new58(A,B,C,D,E,F,G,H,I,J,K).
new39(A,B,C,D,E,F,G,H,I,J,K) :- J>=4, new58(A,B,C,D,E,F,G,H,I,J,K).
new37(A,B,C,D,E,F,G,H,I,J,K) :- A=8640, new39(A,B,C,D,E,F,G,H,I,J,K).
new37(A,B,C,D,E,F,G,H,I,J,K) :- A=<8639, new40(A,B,C,D,E,F,G,H,I,J,K).
new37(A,B,C,D,E,F,G,H,I,J,K) :- A>=8641, new40(A,B,C,D,E,F,G,H,I,J,K).
new36(A,B,C,D,E,F,G,H,I,J,K) :- new7(A,B,C,D,E,F,G,H,I,J,K).
new34(A,B,C,D,E,F,G,H,I,J,K) :- L=8640, A=8608, new36(L,B,C,D,E,F,G,H,I,J,K).
new34(A,B,C,D,E,F,G,H,I,J,K) :- A=<8607, new37(A,B,C,D,E,F,G,H,I,J,K).
new34(A,B,C,D,E,F,G,H,I,J,K) :- A>=8609, new37(A,B,C,D,E,F,G,H,I,J,K).
new33(A,B,C,D,E,F,G,H,I,J,K) :- new7(A,B,C,D,E,F,G,H,I,J,K).
new31(A,B,C,D,E,F,G,H,I,J,K) :- L=8608, A=8592, new33(L,B,C,D,E,F,G,H,I,J,K).
new31(A,B,C,D,E,F,G,H,I,J,K) :- A=<8591, new34(A,B,C,D,E,F,G,H,I,J,K).
new31(A,B,C,D,E,F,G,H,I,J,K) :- A>=8593, new34(A,B,C,D,E,F,G,H,I,J,K).
new30(A,B,C,D,E,F,G,H,I,J,K) :- new65(A,B,C,D,E,F,G,H,I,J,L).
new28(A,B,C,D,E,F,G,H,I,J,K) :- A=8576, new30(A,B,C,D,E,F,G,H,I,J,K).
new28(A,B,C,D,E,F,G,H,I,J,K) :- A=<8575, new31(A,B,C,D,E,F,G,H,I,J,K).
new28(A,B,C,D,E,F,G,H,I,J,K) :- A>=8577, new31(A,B,C,D,E,F,G,H,I,J,K).
new27(A,B,C,D,E,F,G,H,I,J,K) :- new7(A,B,C,D,E,F,G,H,I,J,K).
new25(A,B,C,D,E,F,G,H,I,J,K) :- L=8576, A=8560, new27(L,B,C,D,E,F,G,H,I,J,K).
new25(A,B,C,D,E,F,G,H,I,J,K) :- A=<8559, new28(A,B,C,D,E,F,G,H,I,J,K).
new25(A,B,C,D,E,F,G,H,I,J,K) :- A>=8561, new28(A,B,C,D,E,F,G,H,I,J,K).
new24(A,B,C,D,E,F,G,H,I,J,K) :- D=< -2, new71(A,B,C,D,E,F,G,H,I,J,K).
new24(A,B,C,D,E,F,G,H,I,J,K) :- D>=0, new71(A,B,C,D,E,F,G,H,I,J,K).
new24(A,B,C,D,E,F,G,H,I,J,K) :- L=8560, D= -1, new73(L,B,C,D,E,F,G,H,I,J,K).
new22(A,B,C,D,E,F,G,H,I,J,K) :- A=8544, new24(A,B,C,D,E,F,G,H,I,J,K).
new22(A,B,C,D,E,F,G,H,I,J,K) :- A=<8543, new25(A,B,C,D,E,F,G,H,I,J,K).
new22(A,B,C,D,E,F,G,H,I,J,K) :- A>=8545, new25(A,B,C,D,E,F,G,H,I,J,K).
new21(A,B,C,D,E,F,G,H,I,J,K) :- new7(A,B,C,D,E,F,G,H,I,J,K).
new19(A,B,C,D,E,F,G,H,I,J,K) :- L=8544, A=8528, new21(L,B,C,D,E,F,G,H,I,J,K).
new19(A,B,C,D,E,F,G,H,I,J,K) :- A=<8527, new22(A,B,C,D,E,F,G,H,I,J,K).
new19(A,B,C,D,E,F,G,H,I,J,K) :- A>=8529, new22(A,B,C,D,E,F,G,H,I,J,K).
new18(A,B,C,D,E,F,G,H,I,J,K) :- new7(A,B,C,D,E,F,G,H,I,J,K).
new16(A,B,C,D,E,F,G,H,I,J,K) :- L=8528, A=8512, new18(L,B,C,D,E,F,G,H,I,J,K).
new16(A,B,C,D,E,F,G,H,I,J,K) :- A=<8511, new19(A,B,C,D,E,F,G,H,I,J,K).
new16(A,B,C,D,E,F,G,H,I,J,K) :- A>=8513, new19(A,B,C,D,E,F,G,H,I,J,K).
new15(A,B,C,D,E,F,G,H,I,J,K) :- L=2, J=1, new87(A,B,C,D,E,F,G,H,I,L,K).
new15(A,B,C,D,E,F,G,H,I,J,K) :- J=<0, new87(A,B,C,D,E,F,G,H,I,J,K).
new15(A,B,C,D,E,F,G,H,I,J,K) :- J>=2, new87(A,B,C,D,E,F,G,H,I,J,K).
new13(A,B,C,D,E,F,G,H,I,J,K) :- A=8496, new15(A,B,C,D,E,F,G,H,I,J,K).
new13(A,B,C,D,E,F,G,H,I,J,K) :- A=<8495, new16(A,B,C,D,E,F,G,H,I,J,K).
new13(A,B,C,D,E,F,G,H,I,J,K) :- A>=8497, new16(A,B,C,D,E,F,G,H,I,J,K).
new12(A,B,C,D,E,F,G,H,I,J,K) :- L=8496, M=1, J=0, new94(L,B,C,D,E,F,G,H,I,M,K).
new12(A,B,C,D,E,F,G,H,I,J,K) :- L=8496, J=< -1, new94(L,B,C,D,E,F,G,H,I,J,K).
new12(A,B,C,D,E,F,G,H,I,J,K) :- L=8496, J>=1, new94(L,B,C,D,E,F,G,H,I,J,K).
new11(A,B,C,D,E,F,G,H,I,J,K) :- A=8466, new12(A,B,C,D,E,F,G,H,I,J,K).
new11(A,B,C,D,E,F,G,H,I,J,K) :- A=<8465, new13(A,B,C,D,E,F,G,H,I,J,K).
new11(A,B,C,D,E,F,G,H,I,J,K) :- A>=8467, new13(A,B,C,D,E,F,G,H,I,J,K).
new10(A,B,C,D,E,F,G,H,I,J,K) :- J>=3.
new10(A,B,C,D,E,F,G,H,I,J,K) :- J=<2, new11(A,B,C,D,E,F,G,H,I,J,K).
new9(A,B,C,D,E,F,G,H,I,J,K) :- A=<8512, new10(A,B,C,D,E,F,G,H,I,J,K).
new9(A,B,C,D,E,F,G,H,I,J,K) :- A>=8513, new11(A,B,C,D,E,F,G,H,I,J,K).
new7(A,B,C,D,E,F,G,H,I,J,K) :- new9(A,B,C,D,E,F,G,H,I,J,K).
new6(A,B,C,D,E,F,G,H,I,J,K) :- L=0, M=8466, N=4294967296-O, O=< -1, 
          new7(M,B,C,D,E,F,G,N,O,L,K).
new6(A,B,C,D,E,F,G,H,I,J,K) :- L=0, M=8466, N>=0, new7(M,B,C,D,E,F,G,N,N,L,K).
new5(A,B,C,D,E,F,G,H,I,J,K) :- new6(A,B,C,D,E,L,L,H,I,J,K).
new4(A,B,C,D,E,F,G,H,I,J,K) :- new5(A,B,C,L,L,F,G,H,I,J,K).
new3(A,B,C,D,E,F,G,H,I,J,K) :- new4(A,L,L,D,E,F,G,H,I,J,K).
new2 :- new3(A,B,C,D,E,F,G,H,I,J,K).
new1 :- new2.
false :- new1.
