new30(A,B,C,D) :- B=0.
new29(A,B,C) :- D=1, B=<3, new30(A,D,B,C).
new29(A,B,C) :- D=0, B>=4, new30(A,D,B,C).
new28(A,B,C) :- B>=4, new29(A,B,C).
new27(A,B,C) :- C=<0, new28(A,B,C).
new26(A,B,C) :- C>=0, new27(A,B,C).
new22(A,B,C) :- D=2+B, E=2+C, A=< -1, new22(A,D,E).
new22(A,B,C) :- D=2+B, E=2+C, A>=1, new22(A,D,E).
new22(A,B,C) :- A=0, new26(A,B,C).
new21(A,B,C,D) :- new21(A,B,C,D).
new19(A,B,C,D) :- B=0, new21(A,B,C,D).
new19(A,B,C,D) :- B=< -1, new22(A,C,D).
new19(A,B,C,D) :- B>=1, new22(A,C,D).
new17(A,B,C) :- D=1, C=<2, new19(A,D,B,C).
new17(A,B,C) :- D=0, C>=3, new19(A,D,B,C).
new16(A,B,C,D) :- new16(A,B,C,D).
new14(A,B,C,D) :- B=0, new16(A,B,C,D).
new14(A,B,C,D) :- B=< -1, new17(A,C,D).
new14(A,B,C,D) :- B>=1, new17(A,C,D).
new12(A,B,C) :- D=1, C>=0, new14(A,D,B,C).
new12(A,B,C) :- D=0, C=< -1, new14(A,D,B,C).
new11(A,B,C,D) :- new11(A,B,C,D).
new9(A,B,C,D) :- B=0, new11(A,B,C,D).
new9(A,B,C,D) :- B=< -1, new12(A,C,D).
new9(A,B,C,D) :- B>=1, new12(A,C,D).
new7(A,B,C) :- D=1, B=<2, new9(A,D,B,C).
new7(A,B,C) :- D=0, B>=3, new9(A,D,B,C).
new6(A,B,C,D) :- new6(A,B,C,D).
new4(A,B,C,D) :- B=0, new6(A,B,C,D).
new4(A,B,C,D) :- B=< -1, new7(A,C,D).
new4(A,B,C,D) :- B>=1, new7(A,C,D).
new3(A,B,C) :- D=1, B>=0, new4(A,D,B,C).
new3(A,B,C) :- D=0, B=< -1, new4(A,D,B,C).
new2(A) :- new3(A,B,C).
new1 :- new2(A).
false :- new1.
