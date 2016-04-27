new47(A,B,C) :- D=0, B-C=<1, new4(A,B,D).
new47(A,B,C) :- B-C>=2, new4(A,B,C).
new45(A,B,C,D) :- A=0.
new45(A,B,C,D) :- E=1+D, A=< -1, new47(B,C,E).
new45(A,B,C,D) :- E=1+D, A>=1, new47(B,C,E).
new43(A,B,C) :- D=1, B-C>=1, new45(D,A,B,C).
new43(A,B,C) :- D=0, B-C=<0, new45(D,A,B,C).
new41(A,B,C,D) :- A=0.
new41(A,B,C,D) :- A=< -1, new43(B,C,D).
new41(A,B,C,D) :- A>=1, new43(B,C,D).
new39(A,B,C) :- D=1, C>=0, new41(D,A,B,C).
new39(A,B,C) :- D=0, C=< -1, new41(D,A,B,C).
new37(A,B,C) :- D=0, B-C=<1, new39(A,B,D).
new37(A,B,C) :- B-C>=2, new39(A,B,C).
new35(A,B,C,D) :- A=0.
new35(A,B,C,D) :- E=1+D, A=< -1, new37(B,C,E).
new35(A,B,C,D) :- E=1+D, A>=1, new37(B,C,E).
new33(A,B,C) :- D=1, B-C>=1, new35(D,A,B,C).
new33(A,B,C) :- D=0, B-C=<0, new35(D,A,B,C).
new31(A,B,C,D) :- A=0.
new31(A,B,C,D) :- A=< -1, new33(B,C,D).
new31(A,B,C,D) :- A>=1, new33(B,C,D).
new29(A,B,C) :- D=1, C>=0, new31(D,A,B,C).
new29(A,B,C) :- D=0, C=< -1, new31(D,A,B,C).
new27(A,B,C) :- D=0, B-C=<1, new29(A,B,D).
new27(A,B,C) :- B-C>=2, new29(A,B,C).
new25(A,B,C,D) :- A=0.
new25(A,B,C,D) :- E=1+D, A=< -1, new27(B,C,E).
new25(A,B,C,D) :- E=1+D, A>=1, new27(B,C,E).
new23(A,B,C) :- D=1, B-C>=1, new25(D,A,B,C).
new23(A,B,C) :- D=0, B-C=<0, new25(D,A,B,C).
new21(A,B,C,D) :- A=0.
new21(A,B,C,D) :- A=< -1, new23(B,C,D).
new21(A,B,C,D) :- A>=1, new23(B,C,D).
new20(A,B,C) :- D=1, C>=0, new21(D,A,B,C).
new20(A,B,C) :- D=0, C=< -1, new21(D,A,B,C).
new17(A,B,C) :- A=< -1, new7(A,B,C).
new17(A,B,C) :- A>=1, new7(A,B,C).
new17(A,B,C) :- A=0, new20(A,B,C).
new13(A,B,C,D) :- A=0.
new11(A,B,C) :- D=1, B-C>=1, new13(D,A,B,C).
new11(A,B,C) :- D=0, B-C=<0, new13(D,A,B,C).
new9(A,B,C,D) :- A=0.
new9(A,B,C,D) :- A=< -1, new11(B,C,D).
new9(A,B,C,D) :- A>=1, new11(B,C,D).
new8(A,B,C) :- D=1, C>=0, new9(D,A,B,C).
new8(A,B,C) :- D=0, C=< -1, new9(D,A,B,C).
new7(A,B,C) :- C>=1, new8(A,B,C).
new5(A,B,C) :- A=< -1, new7(A,B,C).
new5(A,B,C) :- A>=1, new7(A,B,C).
new5(A,B,C) :- A=0, new17(A,B,C).
new4(A,B,C) :- A=< -1, new5(A,B,C).
new4(A,B,C) :- A>=1, new5(A,B,C).
new4(A,B,C) :- A=0, new7(A,B,C).
new3(A,B,C) :- D=0, B>=1, new4(A,B,D).
new2 :- new3(A,B,C).
new1 :- new2.
false :- new1.
