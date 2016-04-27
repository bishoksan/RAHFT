new36(A,B,C,D,E,F,G,H,I,J,K,L,M) :- D=< -1, new11(F,G,H,I,J,K,L,M).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M) :- D>=1, new11(F,G,H,I,J,K,L,M).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M) :- D=0, new11(F,G,H,I,J,K,L,M).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1+A, B=< -1, 
          new36(N,B,C,D,E,F,G,H,I,J,K,L,M).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1+A, B>=1, 
          new36(N,B,C,D,E,F,G,H,I,J,K,L,M).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M) :- B=0, new36(A,B,C,D,E,F,G,H,I,J,K,L,M).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M) :- new35(A,B,C,N,N,F,G,H,I,J,K,L,M).
new33(A,B,C,D,E,F,G,H,I,J,K,L,M) :- new34(A,N,N,D,E,F,G,H,I,J,K,L,M).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M) :- D=< -1, new16(F,G,H,I,J,K,L,M).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M) :- D>=1, new16(F,G,H,I,J,K,L,M).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M) :- D=0, new16(F,G,H,I,J,K,L,M).
new26(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1+A, B=< -1, 
          new27(N,B,C,D,E,F,G,H,I,J,K,L,M).
new26(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1+A, B>=1, 
          new27(N,B,C,D,E,F,G,H,I,J,K,L,M).
new26(A,B,C,D,E,F,G,H,I,J,K,L,M) :- B=0, new27(A,B,C,D,E,F,G,H,I,J,K,L,M).
new25(A,B,C,D,E,F,G,H,I,J,K,L,M) :- new26(A,B,C,N,N,F,G,H,I,J,K,L,M).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M) :- new25(A,N,N,D,E,F,G,H,I,J,K,L,M).
new22(A,B,C,D,E,F,G,H,I) :- A=0.
new21(A,B,C,D,E,F,G,H) :- I=1, B=<0, new22(I,A,B,C,D,E,F,G,H).
new21(A,B,C,D,E,F,G,H) :- I=0, B>=1, new22(I,A,B,C,D,E,F,G,H).
new17(A,B,C,D,E,F,G,H) :- I= -(A)+B, B>=1, new17(A,I,C,D,E,F,G,H).
new17(A,B,C,D,E,F,G,H) :- B=<0, new21(A,B,C,D,E,F,G,H).
new16(A,B,C,D,E,F,G,H) :- I= -1+A, E=< -1, new17(I,B,C,D,E,F,G,H).
new16(A,B,C,D,E,F,G,H) :- I= -1+A, E>=1, new17(I,B,C,D,E,F,G,H).
new16(A,B,C,D,E,F,G,H) :- E=0, new17(A,B,C,D,E,F,G,H).
new14(A,B,C,D,E,F,G,H) :- I=0, new24(I,J,K,L,M,A,B,C,D,E,F,G,H).
new13(A,B,C,D,E,F,G,H) :- C=< -1, new14(A,B,C,D,E,F,G,H).
new13(A,B,C,D,E,F,G,H) :- C>=1, new14(A,B,C,D,E,F,G,H).
new13(A,B,C,D,E,F,G,H) :- C=0, new16(A,B,C,D,E,F,G,H).
new12(A,B,C,D,E,F,G,H) :- new13(A,B,C,D,I,F,G,I).
new11(A,B,C,D,E,F,G,H) :- new12(A,B,I,D,E,F,I,H).
new9(A,B,C,D,E,F,G,H) :- I=0, new33(I,J,K,L,M,A,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- E=< -1, new9(A,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- E>=1, new9(A,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- E=0, new11(A,B,C,D,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- I= -1+A, C=< -1, new6(I,B,C,D,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- I= -1+A, C>=1, new6(I,B,C,D,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- C=0, new6(A,B,C,D,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- new5(A,B,C,D,I,I,G,H).
new3(A,B,C,D,E,F,G,H) :- new4(A,B,I,I,E,F,G,H).
new2 :- A=1, new3(A,B,C,D,E,F,G,H).
new1 :- new2.
false :- new1.
