new34(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1+D, C-D>=1, 
          new34(A,B,C,N,E,F,G,H,I,J,K,L,M).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1+E, C-D=<0, 
          new6(A,B,C,D,N,F,G,H,I,J,K,L,M).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1+I, O=L+M, A=< -1, 
          new21(A,B,C,D,E,F,G,H,N,I,K,L,O).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1+I, O=L+M, A>=1, 
          new21(A,B,C,D,E,F,G,H,N,I,K,L,O).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1+I, O=L+M, A=0, 
          new21(A,B,C,D,E,F,G,H,N,J,K,L,O).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1+I, A=< -1, 
          new23(A,B,C,D,E,F,G,H,N,I,K,L,M).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1+I, A>=1, 
          new23(A,B,C,D,E,F,G,H,N,I,K,L,M).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1+I, A=0, 
          new23(A,B,C,D,E,F,G,H,N,J,K,L,M).
new23(A,B,C,D,E,F,G,H,I,J,K,L,M) :- C-E-I>=1, new24(A,B,C,D,E,F,G,H,I,J,K,L,M).
new23(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=E+J, C-E-I=<0, 
          new16(A,B,C,D,E,F,N,H,I,J,K,L,M).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M) :- C-E-I>=1, new29(A,B,C,D,E,F,G,H,I,J,K,L,M).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=E+J, C-E-I=<0, 
          new16(A,B,C,D,E,F,N,H,I,J,K,L,M).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1, O=1+L, L=<0, 
          new21(A,B,C,D,E,F,G,H,N,J,K,L,O).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1, O=1+L, L>=2, 
          new21(A,B,C,D,E,F,G,H,N,J,K,L,O).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1, O=0, L=1, 
          new23(A,B,C,D,E,F,G,H,N,O,K,L,M).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=0, C=1+E, 
          new16(A,B,C,D,E,F,E,H,I,N,K,L,M).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M) :- C-E=<0, new19(A,B,C,D,E,F,G,H,I,J,K,L,M).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M) :- C-E>=2, new19(A,B,C,D,E,F,G,H,I,J,K,L,M).
new16(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A=< -1, new34(A,B,C,F,E,F,G,H,I,J,K,L,M).
new16(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A>=1, new34(A,B,C,F,E,F,G,H,I,J,K,L,M).
new16(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1+E, A=0, new6(A,B,C,D,N,F,G,H,I,J,K,L,M).
new14(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N= -1, O= -1+E, C-E=<0, 
          new16(A,B,C,D,E,F,O,H,I,N,K,L,M).
new14(A,B,C,D,E,F,G,H,I,J,K,L,M) :- C-E>=1, new17(A,B,C,D,E,F,G,H,I,J,K,L,M).
new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- B=0.
new10(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1, B-C>=1, 
          new12(A,N,B,C,D,E,F,G,H,I,J,K,L,M).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=0, B-C=<0, 
          new12(A,N,B,C,D,E,F,G,H,I,J,K,L,M).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- B=0.
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- B=< -1, new10(A,C,D,E,F,G,H,I,J,K,L,M,N).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- B>=1, new10(A,C,D,E,F,G,H,I,J,K,L,M,N).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1, C>=0, new8(A,N,B,C,D,E,F,G,H,I,J,K,L,M).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=0, C=< -1, 
          new8(A,N,B,C,D,E,F,G,H,I,J,K,L,M).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1, O=1+E, E-H=< -1, 
          new14(A,B,C,D,E,O,G,H,I,J,E,N,M).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M) :- E-H>=0, new7(A,B,C,D,E,F,G,H,I,J,K,L,M).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=0, H>=0, new6(A,B,C,D,N,F,G,H,I,J,K,L,M).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M) :- H=< -1, new7(A,B,C,D,E,F,G,H,I,J,K,L,M).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N= -1+C, C>=0, 
          new5(A,B,C,D,E,F,G,N,I,J,K,L,M).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M) :- B-C>=1, new4(A,B,C,D,E,F,G,H,I,J,K,L,M).
new2(A) :- new3(A,B,C,D,E,F,G,H,I,J,K,L,M).
new1 :- new2(A).
false :- new1.
