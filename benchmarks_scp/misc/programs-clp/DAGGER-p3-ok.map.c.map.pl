new176(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- A=0.
new176(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=1+L, A=< -1, 
          new8(B,C,D,E,F,G,H,I,J,K,O,M,N).
new176(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=1+L, A>=1, 
          new8(B,C,D,E,F,G,H,I,J,K,O,M,N).
new174(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1, C-K>=1, 
          new176(N,A,B,C,D,E,F,G,H,I,J,K,L,M).
new174(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=0, C-K=<0, 
          new176(N,A,B,C,D,E,F,G,H,I,J,K,L,M).
new172(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- A=0.
new172(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- A=< -1, 
          new174(B,C,D,E,F,G,H,I,J,K,L,M,N).
new172(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- A>=1, new174(B,C,D,E,F,G,H,I,J,K,L,M,N).
new171(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1, K>=0, 
          new172(N,A,B,C,D,E,F,G,H,I,J,K,L,M).
new171(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=0, K=< -1, 
          new172(N,A,B,C,D,E,F,G,H,I,J,K,L,M).
new168(A,B,C,D,E,F,G,H,I,J,K,L,M) :- C-K=<5, new170(A,B,C,D,E,F,G,H,I,J,K,L,M).
new168(A,B,C,D,E,F,G,H,I,J,K,L,M) :- C-K>=6, new171(A,B,C,D,E,F,G,H,I,J,K,L,M).
new165(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=62, G>=1, 
          new14(N,B,C,D,E,F,G,H,I,J,K,L,M).
new165(A,B,C,D,E,F,G,H,I,J,K,L,M) :- G=<0, new97(A,B,C,D,E,F,G,H,I,J,K,L,M).
new162(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=41, F>=1, 
          new14(N,B,C,D,E,F,G,H,I,J,K,L,M).
new162(A,B,C,D,E,F,G,H,I,J,K,L,M) :- F=<0, new165(A,B,C,D,E,F,G,H,I,J,K,L,M).
new156(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=62, E=0, 
          new14(N,B,C,D,E,F,G,H,I,J,K,L,M).
new156(A,B,C,D,E,F,G,H,I,J,K,L,M) :- E>=1, new14(A,B,C,D,E,F,G,H,I,J,K,L,M).
new155(A,B,C,D,E,F,G,H,I,J,K,L,M) :- B=44, new156(A,B,C,D,E,F,G,H,I,J,K,L,M).
new155(A,B,C,D,E,F,G,H,I,J,K,L,M) :- B=<43, new14(A,B,C,D,E,F,G,H,I,J,K,L,M).
new155(A,B,C,D,E,F,G,H,I,J,K,L,M) :- B>=45, new14(A,B,C,D,E,F,G,H,I,J,K,L,M).
new151(A,B,C,D,E,F,G,H,I,J,K,L,M) :- G=<0, new97(A,B,C,D,E,F,G,H,I,J,K,L,M).
new151(A,B,C,D,E,F,G,H,I,J,K,L,M) :- G>=1, new155(A,B,C,D,E,F,G,H,I,J,K,L,M).
new149(A,B,C,D,E,F,G,H,I,J,K,L,M) :- H=<1, new151(A,B,C,D,E,F,G,H,I,J,K,L,M).
new149(A,B,C,D,E,F,G,H,I,J,K,L,M) :- H>=3, new151(A,B,C,D,E,F,G,H,I,J,K,L,M).
new149(A,B,C,D,E,F,G,H,I,J,K,L,M) :- H=2, new14(A,B,C,D,E,F,G,H,I,J,K,L,M).
new145(A,B,C,D,E,F,G,H,I,J,K,L,M) :- new4(A,B,C,D,E,F,G,H,I,J,K,L,M).
new143(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- A=0.
new143(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=1+L, A=< -1, 
          new145(B,C,D,E,F,G,H,I,J,K,O,M,N).
new143(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=1+L, A>=1, 
          new145(B,C,D,E,F,G,H,I,J,K,O,M,N).
new141(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1, C-K>=1, 
          new143(N,A,B,C,D,E,F,G,H,I,J,K,L,M).
new141(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=0, C-K=<0, 
          new143(N,A,B,C,D,E,F,G,H,I,J,K,L,M).
new139(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- A=0.
new139(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- A=< -1, 
          new141(B,C,D,E,F,G,H,I,J,K,L,M,N).
new139(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- A>=1, new141(B,C,D,E,F,G,H,I,J,K,L,M,N).
new138(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1, K>=0, 
          new139(N,A,B,C,D,E,F,G,H,I,J,K,L,M).
new138(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=0, K=< -1, 
          new139(N,A,B,C,D,E,F,G,H,I,J,K,L,M).
new134(A,B,C,D,E,F,G,H,I,J,K,L,M) :- H=2, new132(A,B,C,D,E,F,G,H,I,J,K,L,M).
new134(A,B,C,D,E,F,G,H,I,J,K,L,M) :- H=<1, new26(A,B,C,D,E,F,G,H,I,J,K,L,M).
new134(A,B,C,D,E,F,G,H,I,J,K,L,M) :- H>=3, new26(A,B,C,D,E,F,G,H,I,J,K,L,M).
new132(A,B,C,D,E,F,G,H,I,J,K,L,M) :- C-K>=6, new138(A,B,C,D,E,F,G,H,I,J,K,L,M).
new131(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A=<32, new132(A,B,C,D,E,F,G,H,I,J,K,L,M).
new131(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A>=34, new132(A,B,C,D,E,F,G,H,I,J,K,L,M).
new131(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A=33, new134(A,B,C,D,E,F,G,H,I,J,K,L,M).
new130(A,B,C,D,E,F,G,H,I,J,K,L,M) :- new4(A,B,C,D,E,F,G,H,I,J,K,L,M).
new124(A,B,C,D,E,F,G,H,I,J,K,L,M) :- H=<1, new97(A,B,C,D,E,F,G,H,I,J,K,L,M).
new124(A,B,C,D,E,F,G,H,I,J,K,L,M) :- H>=3, new97(A,B,C,D,E,F,G,H,I,J,K,L,M).
new124(A,B,C,D,E,F,G,H,I,J,K,L,M) :- H=2, new86(A,B,C,D,E,F,G,H,I,J,K,L,M).
new119(A,B,C,D,E,F,G,H,I,J,K,L,M) :- L=< -1, new120(A,B,C,D,E,F,G,H,I,J,K,L,M).
new119(A,B,C,D,E,F,G,H,I,J,K,L,M) :- L>=1, new120(A,B,C,D,E,F,G,H,I,J,K,L,M).
new119(A,B,C,D,E,F,G,H,I,J,K,L,M) :- L=0, new103(A,B,C,D,E,F,G,H,I,J,K,L,M).
new117(A,B,C,D,E,F,G,H,I,J,K,L,M) :- new119(A,B,C,D,E,F,G,H,I,J,K,N,M).
new115(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- A=0.
new115(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=1+L, A=< -1, 
          new117(B,C,D,E,F,G,H,I,J,K,O,M,N).
new115(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=1+L, A>=1, 
          new117(B,C,D,E,F,G,H,I,J,K,O,M,N).
new113(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1, C-K>=1, 
          new115(N,A,B,C,D,E,F,G,H,I,J,K,L,M).
new113(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=0, C-K=<0, 
          new115(N,A,B,C,D,E,F,G,H,I,J,K,L,M).
new111(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- A=0.
new111(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- A=< -1, 
          new113(B,C,D,E,F,G,H,I,J,K,L,M,N).
new111(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- A>=1, new113(B,C,D,E,F,G,H,I,J,K,L,M,N).
new110(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1, K>=0, 
          new111(N,A,B,C,D,E,F,G,H,I,J,K,L,M).
new110(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=0, K=< -1, 
          new111(N,A,B,C,D,E,F,G,H,I,J,K,L,M).
new106(A,B,C,D,E,F,G,H,I,J,K,L,M) :- G>=1, new3(A,B,C,D,E,F,G,H,I,J,K,L,M).
new104(A,A,B,C,D,E,F,G,H,I,J,K,L) :- new106(A,A,B,C,D,E,F,G,H,I,J,K,L).
new104(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A-B=< -1, new3(A,B,C,D,E,F,G,H,I,J,K,L,M).
new104(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A-B>=1, new3(A,B,C,D,E,F,G,H,I,J,K,L,M).
new103(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A=< -1, new104(A,B,C,D,E,F,G,H,I,J,K,L,M).
new103(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A>=1, new104(A,B,C,D,E,F,G,H,I,J,K,L,M).
new101(A,B,C,D,E,F,G,H,I,J,K,L,M) :- C-K>=6, new110(A,B,C,D,E,F,G,H,I,J,K,L,M).
new100(A,B,C,D,E,F,G,H,I,J,K,L,M) :- L=< -1, new101(A,B,C,D,E,F,G,H,I,J,K,L,M).
new100(A,B,C,D,E,F,G,H,I,J,K,L,M) :- L>=1, new101(A,B,C,D,E,F,G,H,I,J,K,L,M).
new100(A,B,C,D,E,F,G,H,I,J,K,L,M) :- L=0, new103(A,B,C,D,E,F,G,H,I,J,K,L,M).
new97(A,B,C,D,E,F,G,H,I,J,K,L,M) :- new100(A,B,C,D,E,F,G,H,I,J,K,N,M).
new96(A,B,C,D,E,F,G,H,I,J,K,L,M) :- L=< -1, new97(A,B,C,D,E,F,G,H,I,J,K,L,M).
new96(A,B,C,D,E,F,G,H,I,J,K,L,M) :- L>=1, new97(A,B,C,D,E,F,G,H,I,J,K,L,M).
new96(A,B,C,D,E,F,G,H,I,J,K,L,M) :- L=0, new4(A,B,C,D,E,F,G,H,I,J,K,L,M).
new93(A,B,C,D,E,F,G,H,I,J,K,L,M) :- new96(A,B,C,D,E,F,G,H,I,J,K,N,M).
new92(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N= -1, L=< -1, 
          new93(N,B,C,D,E,F,G,H,I,J,K,L,M).
new92(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N= -1, L>=1, 
          new93(N,B,C,D,E,F,G,H,I,J,K,L,M).
new92(A,B,C,D,E,F,G,H,I,J,K,L,M) :- L=0, new93(A,B,C,D,E,F,G,H,I,J,K,L,M).
new90(A,B,C,D,E,F,G,H,I,J,K,L,M) :- new92(A,B,C,D,E,F,G,H,I,J,K,N,M).
new89(A,B,C,D,E,F,G,H,I,J,K,L,M) :- new90(A,B,C,D,E,F,G,H,I,J,K,N,M).
new88(A,B,C,D,E,F,G,H,I,J,K,L,M) :- H=5, new89(A,B,C,D,E,F,G,H,I,J,K,L,M).
new88(A,B,C,D,E,F,G,H,I,J,K,L,M) :- H=<4, new90(A,B,C,D,E,F,G,H,I,J,K,L,M).
new88(A,B,C,D,E,F,G,H,I,J,K,L,M) :- H>=6, new90(A,B,C,D,E,F,G,H,I,J,K,L,M).
new86(A,B,C,D,E,F,G,H,I,J,K,L,M) :- new88(A,B,C,D,E,F,G,N,I,J,K,L,M).
new85(A,B,C,D,E,F,G,H,I,J,K,L,M) :- G=<0, new124(A,B,C,D,E,F,G,H,I,J,K,L,M).
new85(A,B,C,D,E,F,G,H,I,J,K,L,M) :- G>=1, new86(A,B,C,D,E,F,G,H,I,J,K,L,M).
new83(A,A,B,C,D,E,F,G,H,I,J,K,L) :- new85(A,A,B,C,D,E,F,G,H,I,J,K,L).
new83(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A-B=< -1, new86(A,B,C,D,E,F,G,H,I,J,K,L,M).
new83(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A-B>=1, new86(A,B,C,D,E,F,G,H,I,J,K,L,M).
new82(A,B,C,D,E,F,G,H,I,J,K,L,M) :- new4(A,B,C,D,E,F,G,H,I,J,K,L,M).
new78(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N= -1, O=1+F, L=< -1, 
          new27(N,B,C,D,E,O,G,H,I,J,K,L,M).
new78(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N= -1, O=1+F, L>=1, 
          new27(N,B,C,D,E,O,G,H,I,J,K,L,M).
new78(A,B,C,D,E,F,G,H,I,J,K,L,M) :- L=0, new34(A,B,C,D,E,F,G,H,I,J,K,L,M).
new73(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N= -1, F=<0, 
          new27(N,B,C,D,E,F,G,H,I,J,K,L,M).
new73(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N= -1+F, F>=1, 
          new27(A,B,C,D,E,N,G,H,I,J,K,L,M).
new72(A,B,C,D,E,F,G,H,I,J,K,L,M) :- L=< -1, new73(A,B,C,D,E,F,G,H,I,J,K,L,M).
new72(A,B,C,D,E,F,G,H,I,J,K,L,M) :- L>=1, new73(A,B,C,D,E,F,G,H,I,J,K,L,M).
new72(A,B,C,D,E,F,G,H,I,J,K,L,M) :- L=0, new37(A,B,C,D,E,F,G,H,I,J,K,L,M).
new68(A,B,C,D,E,F,G,H,I,J,K,L,M) :- L=< -1, new63(A,B,C,D,E,F,G,H,I,J,K,L,M).
new68(A,B,C,D,E,F,G,H,I,J,K,L,M) :- L>=1, new63(A,B,C,D,E,F,G,H,I,J,K,L,M).
new68(A,B,C,D,E,F,G,H,I,J,K,L,M) :- L=0, new41(A,B,C,D,E,F,G,H,I,J,K,L,M).
new64(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1, L=< -1, 
          new27(A,B,C,D,N,F,G,H,I,J,K,L,M).
new64(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1, L>=1, new27(A,B,C,D,N,F,G,H,I,J,K,L,M).
new64(A,B,C,D,E,F,G,H,I,J,K,L,M) :- L=0, new27(A,B,C,D,E,F,G,H,I,J,K,L,M).
new63(A,B,C,D,E,F,G,H,I,J,K,L,M) :- new64(A,B,C,D,E,F,G,H,I,J,K,N,M).
new61(A,B,C,D,E,F,G,H,I,J,K,L,M) :- new68(A,B,C,D,E,F,G,H,I,J,K,N,M).
new60(A,B,C,D,E,F,G,H,I,J,K,L,M) :- L=< -1, new61(A,B,C,D,E,F,G,H,I,J,K,L,M).
new60(A,B,C,D,E,F,G,H,I,J,K,L,M) :- L>=1, new61(A,B,C,D,E,F,G,H,I,J,K,L,M).
new60(A,B,C,D,E,F,G,H,I,J,K,L,M) :- L=0, new63(A,B,C,D,E,F,G,H,I,J,K,L,M).
new54(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=32, L=< -1, 
          new27(N,B,C,D,E,F,G,H,I,J,K,L,M).
new54(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=32, L>=1, 
          new27(N,B,C,D,E,F,G,H,I,J,K,L,M).
new54(A,B,C,D,E,F,G,H,I,J,K,L,M) :- L=0, new27(A,B,C,D,E,F,G,H,I,J,K,L,M).
new51(A,B,C,D,E,F,G,H,I,J,K,L,M) :- new54(A,B,C,D,E,F,G,H,I,J,K,N,M).
new50(A,B,C,D,E,F,G,H,I,J,K,L,M) :- L=< -1, new51(A,B,C,D,E,F,G,H,I,J,K,L,M).
new50(A,B,C,D,E,F,G,H,I,J,K,L,M) :- L>=1, new51(A,B,C,D,E,F,G,H,I,J,K,L,M).
new50(A,B,C,D,E,F,G,H,I,J,K,L,M) :- L=0, new27(A,B,C,D,E,F,G,H,I,J,K,L,M).
new47(A,B,C,D,E,F,G,H,I,J,K,L,M) :- new50(A,B,C,D,E,F,G,H,I,J,K,N,M).
new45(A,B,C,D,E,F,G,H,I,J,K,L,M) :- B=32, new47(A,B,C,D,E,F,G,H,I,J,K,L,M).
new45(A,B,C,D,E,F,G,H,I,J,K,L,M) :- B=<31, new27(A,B,C,D,E,F,G,H,I,J,K,L,M).
new45(A,B,C,D,E,F,G,H,I,J,K,L,M) :- B>=33, new27(A,B,C,D,E,F,G,H,I,J,K,L,M).
new44(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=0, O= -1, G=<0, 
          new27(O,B,C,D,N,F,G,H,I,J,K,L,M).
new44(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=0, O= -1+G, G>=1, 
          new27(A,B,C,D,N,F,O,H,I,J,K,L,M).
new42(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A=62, new44(A,B,C,D,E,F,G,H,I,J,K,L,M).
new42(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A=<61, new45(A,B,C,D,E,F,G,H,I,J,K,L,M).
new42(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A>=63, new45(A,B,C,D,E,F,G,H,I,J,K,L,M).
new41(A,B,C,D,E,F,G,H,I,J,K,L,M) :- new60(A,B,C,D,E,F,G,H,I,J,K,N,M).
new40(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1+G, A=60, 
          new41(A,B,C,D,E,F,N,H,I,J,K,L,M).
new40(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A=<59, new42(A,B,C,D,E,F,G,H,I,J,K,L,M).
new40(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A>=61, new42(A,B,C,D,E,F,G,H,I,J,K,L,M).
new37(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N= -1, F>=1, 
          new27(N,B,C,D,E,F,G,H,I,J,K,L,M).
new37(A,B,C,D,E,F,G,H,I,J,K,L,M) :- F=<0, new40(A,B,C,D,E,F,G,H,I,J,K,L,M).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M) :- new72(A,B,C,D,E,F,G,H,I,J,K,N,M).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A=41, new36(A,B,C,D,E,F,G,H,I,J,K,L,M).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A=<40, new37(A,B,C,D,E,F,G,H,I,J,K,L,M).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A>=42, new37(A,B,C,D,E,F,G,H,I,J,K,L,M).
new33(A,B,C,D,E,F,G,H,I,J,K,L,M) :- new78(A,B,C,D,E,F,G,H,I,J,K,N,M).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A=40, new33(A,B,C,D,E,F,G,H,I,J,K,L,M).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A=<39, new34(A,B,C,D,E,F,G,H,I,J,K,L,M).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A>=41, new34(A,B,C,D,E,F,G,H,I,J,K,L,M).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M) :- H=<1, new30(A,B,C,D,E,F,G,H,I,J,K,L,M).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M) :- H>=3, new30(A,B,C,D,E,F,G,H,I,J,K,L,M).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M) :- H=2, new27(A,B,C,D,E,F,G,H,I,J,K,L,M).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A= -1, new82(A,B,C,D,E,F,G,H,I,J,K,L,M).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A=< -2, new83(A,B,C,D,E,F,G,H,I,J,K,L,M).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A>=0, new83(A,B,C,D,E,F,G,H,I,J,K,L,M).
new26(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1, A=92, new27(A,B,C,N,E,F,G,H,I,J,K,L,M).
new26(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A=<91, new28(A,B,C,D,E,F,G,H,I,J,K,L,M).
new26(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A>=93, new28(A,B,C,D,E,F,G,H,I,J,K,L,M).
new25(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N= -1, F>=1, 
          new130(N,B,C,D,E,F,G,H,I,J,K,L,M).
new25(A,B,C,D,E,F,G,H,I,J,K,L,M) :- F=<0, new131(A,B,C,D,E,F,G,H,I,J,K,L,M).
new23(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=0, D>=1, new25(A,B,C,N,E,F,G,H,I,J,K,L,M).
new23(A,B,C,D,E,F,G,H,I,J,K,L,M) :- D=0, new26(A,B,C,D,E,F,G,H,I,J,K,L,M).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- A=0.
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- A=< -1, new23(B,C,D,E,F,G,H,I,J,K,L,M,N).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- A>=1, new23(B,C,D,E,F,G,H,I,J,K,L,M,N).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1, C-K>=1, 
          new21(N,A,B,C,D,E,F,G,H,I,J,K,L,M).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=0, C-K=<0, 
          new21(N,A,B,C,D,E,F,G,H,I,J,K,L,M).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- A=0.
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- A=< -1, new19(B,C,D,E,F,G,H,I,J,K,L,M,N).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- A>=1, new19(B,C,D,E,F,G,H,I,J,K,L,M,N).
new16(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=1, K>=0, 
          new17(N,A,B,C,D,E,F,G,H,I,J,K,L,M).
new16(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=0, K=< -1, 
          new17(N,A,B,C,D,E,F,G,H,I,J,K,L,M).
new14(A,B,C,D,E,F,G,H,I,J,K,L,M) :- C-K>=6, new16(A,B,C,D,E,F,G,H,I,J,K,L,M).
new13(A,B,C,D,E,F,G,H,I,J,K,L,M) :- F=<0, new149(A,B,C,D,E,F,G,H,I,J,K,L,M).
new13(A,B,C,D,E,F,G,H,I,J,K,L,M) :- F>=1, new14(A,B,C,D,E,F,G,H,I,J,K,L,M).
new11(A,A,B,C,D,E,F,G,H,I,J,K,L) :- new13(A,A,B,C,D,E,F,G,H,I,J,K,L).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A-B=< -1, new14(A,B,C,D,E,F,G,H,I,J,K,L,M).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A-B>=1, new14(A,B,C,D,E,F,G,H,I,J,K,L,M).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M) :- N=34, H=2, new14(N,B,C,D,E,F,G,H,I,J,K,L,M).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M) :- H=<1, new162(A,B,C,D,E,F,G,H,I,J,K,L,M).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M) :- H>=3, new162(A,B,C,D,E,F,G,H,I,J,K,L,M).
new9(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A=0, new10(A,B,C,D,E,F,G,H,I,J,K,L,M).
new9(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A=< -1, new11(A,B,C,D,E,F,G,H,I,J,K,L,M).
new9(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A>=1, new11(A,B,C,D,E,F,G,H,I,J,K,L,M).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M) :- new9(N,B,C,D,E,F,G,H,I,J,K,L,M).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M) :- D=0, new168(A,B,C,D,E,F,G,H,I,J,K,L,M).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M) :- D>=1, new8(A,B,C,D,E,F,G,H,I,J,K,L,M).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A=< -2, new6(A,B,C,D,E,F,G,H,I,J,K,L,M).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A>=0, new6(A,B,C,D,E,F,G,H,I,J,K,L,M).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M) :- A= -1, new8(A,B,C,D,E,F,G,H,I,J,K,L,M).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M) :- new5(A,B,C,D,E,F,G,H,I,J,K,L,M).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M) :- new4(A,B,C,D,E,F,G,H,I,J,K,L,M).
new2 :- A= -1, B=0, C=0, D=0, E=0, F=1, G=0, H=0, 
          new3(A,I,J,B,C,D,E,F,G,K,H,L,M).
new1 :- new2.
false :- new1.
