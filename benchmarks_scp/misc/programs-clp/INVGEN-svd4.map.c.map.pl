new159(A,B,C,D,E,F,G,H) :- B=0.
new159(A,B,C,D,E,F,G,H) :- I=1+E, B=< -1, new18(A,C,D,I,F,G,H).
new159(A,B,C,D,E,F,G,H) :- I=1+E, B>=1, new18(A,C,D,I,F,G,H).
new157(A,B,C,D,E,F,G) :- H=1, B-D>=0, new159(A,H,B,C,D,E,F,G).
new157(A,B,C,D,E,F,G) :- H=0, B-D=< -1, new159(A,H,B,C,D,E,F,G).
new155(A,B,C,D,E,F,G,H) :- B=0.
new155(A,B,C,D,E,F,G,H) :- B=< -1, new157(A,C,D,E,F,G,H).
new155(A,B,C,D,E,F,G,H) :- B>=1, new157(A,C,D,E,F,G,H).
new153(A,B,C,D,E,F,G) :- H=1, D>=1, new155(A,H,B,C,D,E,F,G).
new153(A,B,C,D,E,F,G) :- H=0, D=<0, new155(A,H,B,C,D,E,F,G).
new151(A,B,C,D,E,F,G,H) :- B=0.
new151(A,B,C,D,E,F,G,H) :- B=< -1, new153(A,C,D,E,F,G,H).
new151(A,B,C,D,E,F,G,H) :- B>=1, new153(A,C,D,E,F,G,H).
new149(A,B,C,D,E,F,G) :- H=1, C-G=<0, new151(A,H,B,C,D,E,F,G).
new149(A,B,C,D,E,F,G) :- H=0, C-G>=1, new151(A,H,B,C,D,E,F,G).
new147(A,B,C,D,E,F,G,H) :- B=0.
new147(A,B,C,D,E,F,G,H) :- B=< -1, new149(A,C,D,E,F,G,H).
new147(A,B,C,D,E,F,G,H) :- B>=1, new149(A,C,D,E,F,G,H).
new143(A,B,C,D,E,F,G,H) :- B=0.
new143(A,B,C,D,E,F,G,H) :- I=1+F, B=< -1, new59(A,C,D,E,I,G,H).
new143(A,B,C,D,E,F,G,H) :- I=1+F, B>=1, new59(A,C,D,E,I,G,H).
new141(A,B,C,D,E,F,G) :- H=1, B-D>=0, new143(A,H,B,C,D,E,F,G).
new141(A,B,C,D,E,F,G) :- H=0, B-D=< -1, new143(A,H,B,C,D,E,F,G).
new139(A,B,C,D,E,F,G,H) :- B=0.
new139(A,B,C,D,E,F,G,H) :- B=< -1, new141(A,C,D,E,F,G,H).
new139(A,B,C,D,E,F,G,H) :- B>=1, new141(A,C,D,E,F,G,H).
new137(A,B,C,D,E,F,G) :- H=1, D>=1, new139(A,H,B,C,D,E,F,G).
new137(A,B,C,D,E,F,G) :- H=0, D=<0, new139(A,H,B,C,D,E,F,G).
new135(A,B,C,D,E,F,G,H) :- B=0.
new135(A,B,C,D,E,F,G,H) :- B=< -1, new137(A,C,D,E,F,G,H).
new135(A,B,C,D,E,F,G,H) :- B>=1, new137(A,C,D,E,F,G,H).
new133(A,B,C,D,E,F,G) :- H=1, B-C>=0, new135(A,H,B,C,D,E,F,G).
new133(A,B,C,D,E,F,G) :- H=0, B-C=< -1, new135(A,H,B,C,D,E,F,G).
new131(A,B,C,D,E,F,G,H) :- B=0.
new131(A,B,C,D,E,F,G,H) :- B=< -1, new133(A,C,D,E,F,G,H).
new131(A,B,C,D,E,F,G,H) :- B>=1, new133(A,C,D,E,F,G,H).
new129(A,B,C,D,E,F,G) :- H=1, C>=1, new131(A,H,B,C,D,E,F,G).
new129(A,B,C,D,E,F,G) :- H=0, C=<0, new131(A,H,B,C,D,E,F,G).
new127(A,B,C,D,E,F,G,H) :- B=0.
new127(A,B,C,D,E,F,G,H) :- B=< -1, new129(A,C,D,E,F,G,H).
new127(A,B,C,D,E,F,G,H) :- B>=1, new129(A,C,D,E,F,G,H).
new125(A,B,C,D,E,F,G) :- H=1, E-G=<0, new127(A,H,B,C,D,E,F,G).
new125(A,B,C,D,E,F,G) :- H=0, E-G>=1, new127(A,H,B,C,D,E,F,G).
new123(A,B,C,D,E,F,G,H) :- B=0.
new123(A,B,C,D,E,F,G,H) :- B=< -1, new125(A,C,D,E,F,G,H).
new123(A,B,C,D,E,F,G,H) :- B>=1, new125(A,C,D,E,F,G,H).
new119(A,B,C,D,E,F,G,H) :- B=0.
new119(A,B,C,D,E,F,G,H) :- I=1+F, B=< -1, new95(A,C,D,E,I,G,H).
new119(A,B,C,D,E,F,G,H) :- I=1+F, B>=1, new95(A,C,D,E,I,G,H).
new117(A,B,C,D,E,F,G) :- H=1, B-C>=0, new119(A,H,B,C,D,E,F,G).
new117(A,B,C,D,E,F,G) :- H=0, B-C=< -1, new119(A,H,B,C,D,E,F,G).
new115(A,B,C,D,E,F,G,H) :- B=0.
new115(A,B,C,D,E,F,G,H) :- B=< -1, new117(A,C,D,E,F,G,H).
new115(A,B,C,D,E,F,G,H) :- B>=1, new117(A,C,D,E,F,G,H).
new113(A,B,C,D,E,F,G) :- H=1, C>=1, new115(A,H,B,C,D,E,F,G).
new113(A,B,C,D,E,F,G) :- H=0, C=<0, new115(A,H,B,C,D,E,F,G).
new111(A,B,C,D,E,F,G,H) :- B=0.
new111(A,B,C,D,E,F,G,H) :- B=< -1, new113(A,C,D,E,F,G,H).
new111(A,B,C,D,E,F,G,H) :- B>=1, new113(A,C,D,E,F,G,H).
new109(A,B,C,D,E,F,G) :- H=1, B-D>=0, new111(A,H,B,C,D,E,F,G).
new109(A,B,C,D,E,F,G) :- H=0, B-D=< -1, new111(A,H,B,C,D,E,F,G).
new107(A,B,C,D,E,F,G,H) :- B=0.
new107(A,B,C,D,E,F,G,H) :- B=< -1, new109(A,C,D,E,F,G,H).
new107(A,B,C,D,E,F,G,H) :- B>=1, new109(A,C,D,E,F,G,H).
new105(A,B,C,D,E,F,G) :- H=1, D>=1, new107(A,H,B,C,D,E,F,G).
new105(A,B,C,D,E,F,G) :- H=0, D=<0, new107(A,H,B,C,D,E,F,G).
new103(A,B,C,D,E,F,G,H) :- B=0.
new103(A,B,C,D,E,F,G,H) :- B=< -1, new105(A,C,D,E,F,G,H).
new103(A,B,C,D,E,F,G,H) :- B>=1, new105(A,C,D,E,F,G,H).
new101(A,B,C,D,E,F,G) :- H=1, E-G=<0, new103(A,H,B,C,D,E,F,G).
new101(A,B,C,D,E,F,G) :- H=0, E-G>=1, new103(A,H,B,C,D,E,F,G).
new99(A,B,C,D,E,F,G,H) :- B=0.
new99(A,B,C,D,E,F,G,H) :- B=< -1, new101(A,C,D,E,F,G,H).
new99(A,B,C,D,E,F,G,H) :- B>=1, new101(A,C,D,E,F,G,H).
new97(A,B,C,D,E,F,G) :- H=1, E>=1, new99(A,H,B,C,D,E,F,G).
new97(A,B,C,D,E,F,G) :- H=0, E=<0, new99(A,H,B,C,D,E,F,G).
new95(A,B,C,D,E,F,G) :- E-G=<0, new97(A,B,C,D,E,F,G).
new95(A,B,C,D,E,F,G) :- H=1+D, E-G>=1, new22(A,B,C,H,E,F,G).
new93(A,B,C,D,E,F,G,H) :- B=0.
new93(A,B,C,D,E,F,G,H) :- B=< -1, new95(A,C,D,E,D,G,H).
new93(A,B,C,D,E,F,G,H) :- B>=1, new95(A,C,D,E,D,G,H).
new91(A,B,C,D,E,F,G) :- H=1, B-C>=0, new93(A,H,B,C,D,E,F,G).
new91(A,B,C,D,E,F,G) :- H=0, B-C=< -1, new93(A,H,B,C,D,E,F,G).
new89(A,B,C,D,E,F,G,H) :- B=0.
new89(A,B,C,D,E,F,G,H) :- B=< -1, new91(A,C,D,E,F,G,H).
new89(A,B,C,D,E,F,G,H) :- B>=1, new91(A,C,D,E,F,G,H).
new87(A,B,C,D,E,F,G) :- H=1, C>=1, new89(A,H,B,C,D,E,F,G).
new87(A,B,C,D,E,F,G) :- H=0, C=<0, new89(A,H,B,C,D,E,F,G).
new85(A,B,C,D,E,F,G,H) :- B=0.
new85(A,B,C,D,E,F,G,H) :- B=< -1, new87(A,C,D,E,F,G,H).
new85(A,B,C,D,E,F,G,H) :- B>=1, new87(A,C,D,E,F,G,H).
new83(A,B,C,D,E,F,G) :- H=1, C-G=<0, new85(A,H,B,C,D,E,F,G).
new83(A,B,C,D,E,F,G) :- H=0, C-G>=1, new85(A,H,B,C,D,E,F,G).
new81(A,B,C,D,E,F,G,H) :- B=0.
new81(A,B,C,D,E,F,G,H) :- B=< -1, new83(A,C,D,E,F,G,H).
new81(A,B,C,D,E,F,G,H) :- B>=1, new83(A,C,D,E,F,G,H).
new80(A,B,C,D,E,F,G) :- H=1, C>=1, new81(A,H,B,C,D,E,F,G).
new80(A,B,C,D,E,F,G) :- H=0, C=<0, new81(A,H,B,C,D,E,F,G).
new79(A,B,C,D,E,F,G) :- H=1, E>=1, new123(A,H,B,C,D,E,F,G).
new79(A,B,C,D,E,F,G) :- H=0, E=<0, new123(A,H,B,C,D,E,F,G).
new75(A,B,C,D,E,F,G,H) :- B=0.
new75(A,B,C,D,E,F,G,H) :- I=1+E, B=< -1, new60(A,C,D,I,F,G,H).
new75(A,B,C,D,E,F,G,H) :- I=1+E, B>=1, new60(A,C,D,I,F,G,H).
new73(A,B,C,D,E,F,G) :- H=1, B-C>=0, new75(A,H,B,C,D,E,F,G).
new73(A,B,C,D,E,F,G) :- H=0, B-C=< -1, new75(A,H,B,C,D,E,F,G).
new71(A,B,C,D,E,F,G,H) :- B=0.
new71(A,B,C,D,E,F,G,H) :- B=< -1, new73(A,C,D,E,F,G,H).
new71(A,B,C,D,E,F,G,H) :- B>=1, new73(A,C,D,E,F,G,H).
new69(A,B,C,D,E,F,G) :- H=1, C>=1, new71(A,H,B,C,D,E,F,G).
new69(A,B,C,D,E,F,G) :- H=0, C=<0, new71(A,H,B,C,D,E,F,G).
new67(A,B,C,D,E,F,G,H) :- B=0.
new67(A,B,C,D,E,F,G,H) :- B=< -1, new69(A,C,D,E,F,G,H).
new67(A,B,C,D,E,F,G,H) :- B>=1, new69(A,C,D,E,F,G,H).
new65(A,B,C,D,E,F,G) :- H=1, D-G=<0, new67(A,H,B,C,D,E,F,G).
new65(A,B,C,D,E,F,G) :- H=0, D-G>=1, new67(A,H,B,C,D,E,F,G).
new63(A,B,C,D,E,F,G,H) :- B=0.
new63(A,B,C,D,E,F,G,H) :- B=< -1, new65(A,C,D,E,F,G,H).
new63(A,B,C,D,E,F,G,H) :- B>=1, new65(A,C,D,E,F,G,H).
new61(A,B,C,D,E,F,G) :- H=1, D>=1, new63(A,H,B,C,D,E,F,G).
new61(A,B,C,D,E,F,G) :- H=0, D=<0, new63(A,H,B,C,D,E,F,G).
new60(A,B,C,D,E,F,G) :- D-G=<0, new61(A,B,C,D,E,F,G).
new60(A,B,C,D,E,F,G) :- D-G>=1, new26(A,B,C,D,E,F,G).
new59(A,B,C,D,E,F,G) :- E-G=<0, new79(A,B,C,D,E,F,G).
new59(A,B,C,D,E,F,G) :- E-G>=1, new80(A,B,C,D,E,F,G).
new55(A,B,C,D,E,F,G,H) :- B=0.
new55(A,B,C,D,E,F,G,H) :- I=1+E, B=< -1, new24(A,C,D,I,F,G,H).
new55(A,B,C,D,E,F,G,H) :- I=1+E, B>=1, new24(A,C,D,I,F,G,H).
new53(A,B,C,D,E,F,G) :- H=1, B-C>=0, new55(A,H,B,C,D,E,F,G).
new53(A,B,C,D,E,F,G) :- H=0, B-C=< -1, new55(A,H,B,C,D,E,F,G).
new51(A,B,C,D,E,F,G,H) :- B=0.
new51(A,B,C,D,E,F,G,H) :- B=< -1, new53(A,C,D,E,F,G,H).
new51(A,B,C,D,E,F,G,H) :- B>=1, new53(A,C,D,E,F,G,H).
new49(A,B,C,D,E,F,G) :- H=1, C>=1, new51(A,H,B,C,D,E,F,G).
new49(A,B,C,D,E,F,G) :- H=0, C=<0, new51(A,H,B,C,D,E,F,G).
new47(A,B,C,D,E,F,G,H) :- B=0.
new47(A,B,C,D,E,F,G,H) :- B=< -1, new49(A,C,D,E,F,G,H).
new47(A,B,C,D,E,F,G,H) :- B>=1, new49(A,C,D,E,F,G,H).
new45(A,B,C,D,E,F,G) :- H=1, D-G=<0, new47(A,H,B,C,D,E,F,G).
new45(A,B,C,D,E,F,G) :- H=0, D-G>=1, new47(A,H,B,C,D,E,F,G).
new43(A,B,C,D,E,F,G,H) :- B=0.
new43(A,B,C,D,E,F,G,H) :- B=< -1, new45(A,C,D,E,F,G,H).
new43(A,B,C,D,E,F,G,H) :- B>=1, new45(A,C,D,E,F,G,H).
new39(A,B,C,D,E,F,G,H) :- B=0.
new39(A,B,C,D,E,F,G,H) :- I= -1+D, B=< -1, new9(A,C,I,E,F,G,H).
new39(A,B,C,D,E,F,G,H) :- I= -1+D, B>=1, new9(A,C,I,E,F,G,H).
new37(A,B,C,D,E,F,G) :- H=1, B-C>=0, new39(A,H,B,C,D,E,F,G).
new37(A,B,C,D,E,F,G) :- H=0, B-C=< -1, new39(A,H,B,C,D,E,F,G).
new35(A,B,C,D,E,F,G,H) :- B=0.
new35(A,B,C,D,E,F,G,H) :- B=< -1, new37(A,C,D,E,F,G,H).
new35(A,B,C,D,E,F,G,H) :- B>=1, new37(A,C,D,E,F,G,H).
new33(A,B,C,D,E,F,G) :- H=1, C>=1, new35(A,H,B,C,D,E,F,G).
new33(A,B,C,D,E,F,G) :- H=0, C=<0, new35(A,H,B,C,D,E,F,G).
new31(A,B,C,D,E,F,G,H) :- B=0.
new31(A,B,C,D,E,F,G,H) :- B=< -1, new33(A,C,D,E,F,G,H).
new31(A,B,C,D,E,F,G,H) :- B>=1, new33(A,C,D,E,F,G,H).
new29(A,B,C,D,E,F,G) :- H=1, C-G=<0, new31(A,H,B,C,D,E,F,G).
new29(A,B,C,D,E,F,G) :- H=0, C-G>=1, new31(A,H,B,C,D,E,F,G).
new27(A,B,C,D,E,F,G,H) :- B=0.
new27(A,B,C,D,E,F,G,H) :- B=< -1, new29(A,C,D,E,F,G,H).
new27(A,B,C,D,E,F,G,H) :- B>=1, new29(A,C,D,E,F,G,H).
new26(A,B,C,D,E,F,G) :- H=1, C>=1, new27(A,H,B,C,D,E,F,G).
new26(A,B,C,D,E,F,G) :- H=0, C=<0, new27(A,H,B,C,D,E,F,G).
new25(A,B,C,D,E,F,G) :- H=1, D>=1, new43(A,H,B,C,D,E,F,G).
new25(A,B,C,D,E,F,G) :- H=0, D=<0, new43(A,H,B,C,D,E,F,G).
new24(A,B,C,D,E,F,G) :- D-G=<0, new25(A,B,C,D,E,F,G).
new24(A,B,C,D,E,F,G) :- D-G>=1, new26(A,B,C,D,E,F,G).
new22(A,B,C,D,E,F,G) :- B-D>=0, new59(A,B,C,D,F,F,G).
new22(A,B,C,D,E,F,G) :- B-D=< -1, new60(A,B,C,C,E,F,G).
new21(A,B,C,D,E,F,G) :- A=< -1, new22(A,B,C,F,E,F,G).
new21(A,B,C,D,E,F,G) :- A>=1, new22(A,B,C,F,E,F,G).
new21(A,B,C,D,E,F,G) :- A=0, new24(A,B,C,C,E,F,G).
new20(A,B,C,D,E,F,G) :- H=1, C>=1, new147(A,H,B,C,D,E,F,G).
new20(A,B,C,D,E,F,G) :- H=0, C=<0, new147(A,H,B,C,D,E,F,G).
new18(A,B,C,D,E,F,G) :- B-D>=0, new20(A,B,C,D,E,F,G).
new18(A,B,C,D,E,F,G) :- B-D=< -1, new21(A,B,C,D,E,F,G).
new16(A,B,C,D,E,F,G,H) :- B=0.
new16(A,B,C,D,E,F,G,H) :- B=< -1, new18(A,C,D,G,F,G,H).
new16(A,B,C,D,E,F,G,H) :- B>=1, new18(A,C,D,G,F,G,H).
new14(A,B,C,D,E,F,G) :- H=1, B-C>=0, new16(A,H,B,C,D,E,F,G).
new14(A,B,C,D,E,F,G) :- H=0, B-C=< -1, new16(A,H,B,C,D,E,F,G).
new12(A,B,C,D,E,F,G,H) :- B=0.
new12(A,B,C,D,E,F,G,H) :- B=< -1, new14(A,C,D,E,F,G,H).
new12(A,B,C,D,E,F,G,H) :- B>=1, new14(A,C,D,E,F,G,H).
new11(A,B,C,D,E,F,G) :- H=1, C>=1, new12(A,H,B,C,D,E,F,G).
new11(A,B,C,D,E,F,G) :- H=0, C=<0, new12(A,H,B,C,D,E,F,G).
new9(A,B,C,D,E,F,G) :- H=1+C, C>=1, new11(A,B,C,D,E,H,G).
new7(A,B,C,D,E,F,G) :- B-G>=0, new9(A,B,G,D,E,F,G).
new7(A,B,C,D,E,F,G) :- B-G=< -1, new9(A,B,B,D,E,F,G).
new6(A,B,C,D,E,F,G,H) :- new6(A,B,C,D,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- B=0, new6(A,B,C,D,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- B=< -1, new7(A,C,D,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- B>=1, new7(A,C,D,E,F,G,H).
new3(A,B,C,D,E,F,G) :- H=1, B-G>=1, new4(A,H,B,C,D,E,F,G).
new3(A,B,C,D,E,F,G) :- H=0, B-G=<0, new4(A,H,B,C,D,E,F,G).
new2(A) :- new3(A,B,C,D,E,F,G).
new1 :- new2(A).
false :- new1.
