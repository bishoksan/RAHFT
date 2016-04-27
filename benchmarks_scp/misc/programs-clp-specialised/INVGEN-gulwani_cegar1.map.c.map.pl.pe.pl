new30(A,B,C,D) :-
   1*C>=4,
   1*A=0,
   1*B=0,
   1*D=0.
new29(A,B,C) :-
   1*B>=4,
   1*A=0,
   1*C=0,
   1*D=0,
   new30(A,D,B,C).
new28(A,B,C) :-
   1*B>=4,
   1*A=0,
   1*C=0,
   new29(A,B,C).
new27(A,B,C) :-
   1*B>=4,
   1*A=0,
   1*C=0,
   new28(A,B,C).
new26(A,B,C) :-
   1*B>=4,
   1*A=0,
   1*C=0,
   new27(A,B,C).
new22(A,B,C) :-
   1*B>=4,
   1*A=0,
   1*C=0,
   new26(A,B,C).
new19(A,B,C,D) :-
   1*C>=4,
   1*A=0,
   1*B=1,
   1*D=0,
   new22(A,C,D).
new17(A,B,C) :-
   1*B>=4,
   1*A=0,
   1*C=0,
   1*D=1,
   new19(A,D,B,C).
new14(A,B,C,D) :-
   1*C>=4,
   1*A=0,
   1*B=1,
   1*D=0,
   new17(A,C,D).
new12(A,B,C) :-
   1*B>=4,
   1*A=0,
   1*C=0,
   1*D=1,
   new14(A,D,B,C).
new9(A,B,C,D) :-
   1*C>=4,
   1*A=0,
   1*B=1,
   1*D=0,
   new12(A,C,D).

