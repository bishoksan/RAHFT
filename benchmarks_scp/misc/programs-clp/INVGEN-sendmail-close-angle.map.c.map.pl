new39(A,B,C,D,E,F,G) :- A=0.
new39(A,B,C,D,E,F,G) :- A=< -1, new6(B,C,D,E,F,G).
new39(A,B,C,D,E,F,G) :- A>=1, new6(B,C,D,E,F,G).
new37(A,B,C,D,E,F) :- G=1, B-C=< -1, new39(G,A,B,C,D,E,F).
new37(A,B,C,D,E,F) :- G=0, B-C>=0, new39(G,A,B,C,D,E,F).
new35(A,B,C,D,E,F,G) :- A=0.
new35(A,B,C,D,E,F,G) :- A=< -1, new37(B,C,D,E,F,G).
new35(A,B,C,D,E,F,G) :- A>=1, new37(B,C,D,E,F,G).
new33(A,B,C,D,E,F) :- G=1, B>=0, new35(G,A,B,C,D,E,F).
new33(A,B,C,D,E,F) :- G=0, B=< -1, new35(G,A,B,C,D,E,F).
new31(A,B,C,D,E,F,G) :- A=0.
new31(A,B,C,D,E,F,G) :- H=1+C, I=1+F, A=< -1, new33(B,H,D,E,I,G).
new31(A,B,C,D,E,F,G) :- H=1+C, I=1+F, A>=1, new33(B,H,D,E,I,G).
new29(A,B,C,D,E,F) :- G=1, D-E>=1, new31(G,A,B,C,D,E,F).
new29(A,B,C,D,E,F) :- G=0, D-E=<0, new31(G,A,B,C,D,E,F).
new27(A,B,C,D,E,F,G) :- A=0.
new27(A,B,C,D,E,F,G) :- A=< -1, new29(B,C,D,E,F,G).
new27(A,B,C,D,E,F,G) :- A>=1, new29(B,C,D,E,F,G).
new25(A,B,C,D,E,F) :- G=1, E>=0, new27(G,A,B,C,D,E,F).
new25(A,B,C,D,E,F) :- G=0, E=< -1, new27(G,A,B,C,D,E,F).
new22(A,B,C,D,E,F,G) :- A=0.
new20(A,B,C,D,E,F) :- G=1, D-E>=1, new22(G,A,B,C,D,E,F).
new20(A,B,C,D,E,F) :- G=0, D-E=<0, new22(G,A,B,C,D,E,F).
new18(A,B,C,D,E,F,G) :- A=0.
new18(A,B,C,D,E,F,G) :- A=< -1, new20(B,C,D,E,F,G).
new18(A,B,C,D,E,F,G) :- A>=1, new20(B,C,D,E,F,G).
new16(A,B,C,D,E,F) :- G=1, E>=0, new18(G,A,B,C,D,E,F).
new16(A,B,C,D,E,F) :- G=0, E=< -1, new18(G,A,B,C,D,E,F).
new14(A,B,C,D,E,F,G) :- A=0.
new14(A,B,C,D,E,F,G) :- H=1+F, A=< -1, new16(B,C,D,E,H,G).
new14(A,B,C,D,E,F,G) :- H=1+F, A>=1, new16(B,C,D,E,H,G).
new12(A,B,C,D,E,F) :- G=1, D-E>=1, new14(G,A,B,C,D,E,F).
new12(A,B,C,D,E,F) :- G=0, D-E=<0, new14(G,A,B,C,D,E,F).
new10(A,B,C,D,E,F,G) :- A=0.
new10(A,B,C,D,E,F,G) :- A=< -1, new12(B,C,D,E,F,G).
new10(A,B,C,D,E,F,G) :- A>=1, new12(B,C,D,E,F,G).
new9(A,B,C,D,E,F) :- G=1, E>=0, new10(G,A,B,C,D,E,F).
new9(A,B,C,D,E,F) :- G=0, E=< -1, new10(G,A,B,C,D,E,F).
new7(A,B,C,D,E,E) :- new9(A,B,C,D,E,E).
new7(A,B,C,D,E,F) :- E-F=< -1, new25(A,B,C,D,E,F).
new7(A,B,C,D,E,F) :- E-F>=1, new25(A,B,C,D,E,F).
new6(A,B,C,D,E,F) :- A=< -1, new7(A,B,C,D,E,F).
new6(A,B,C,D,E,F) :- A>=1, new7(A,B,C,D,E,F).
new6(A,B,C,D,E,F) :- A=0, new9(A,B,C,D,E,F).
new5(A,B,C,D,E,F) :- G=0, H=0, I= -2+D, C-D>=1, new6(A,G,C,D,H,I).
new4(A,B,C,D,E,F) :- C>=1, new5(A,B,C,D,E,F).
new3(A,B,C,D,E,F) :- D>=2, new4(A,B,C,D,E,F).
new2 :- new3(A,B,C,D,E,F).
new1 :- new2.
false :- new1.
