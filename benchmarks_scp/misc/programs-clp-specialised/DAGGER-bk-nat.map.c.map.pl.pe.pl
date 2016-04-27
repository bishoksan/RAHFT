new28(A,B,C,D,E,F,G,H) :-
   1*C>=0,
   1*D>=0,
   1*E>=0,
   1*B>=0,
   1*A=0,
   1*H=0.
new26(A,B,C,D,E,F,G) :-
   1*C>=0,
   1*A=0,
   1*B=0,
   1*D=0,
   1*G=0,
   1*H=0,
   new28(H,A,B,C,D,E,F,G).
new24(A,B,C,D,E,F,G,H) :-
   1*D>=0,
   1*A=1,
   1*B=0,
   1*C=0,
   1*E=0,
   1*H=0,
   new26(B,C,D,E,F,G,H).
new22(A,B,C,D,E,F,G) :-
   1*C>=0,
   1*A=0,
   1*B=0,
   1*D=0,
   1*G=0,
   1*H=1,
   new24(H,A,B,C,D,E,F,G).
new20(A,B,C,D,E,F,G,H) :-
   1*D>=0,
   1*A=1,
   1*B=0,
   1*C=0,
   1*E=0,
   1*H=0,
   new22(B,C,D,E,F,G,H).
new18(A,B,C,D,E,F,G) :-
   1*C>=0,
   1*A=0,
   1*B=0,
   1*D=0,
   1*G=0,
   1*H=1,
   new20(H,A,B,C,D,E,F,G).
new16(A,B,C,D,E,F,G,H) :-
   1*D>=0,
   1*A=1,
   1*B=0,
   1*C=0,
   1*E=0,
   1*H=0,
   new18(B,C,D,E,F,G,H).
new14(A,B,C,D,E,F,G) :-
   1*C>=0,
   1*A=0,
   1*B=0,
   1*D=0,
   1*G=0,
   1*H=1,
   new16(H,A,B,C,D,E,F,G).
new12(A,B,C,D,E,F,G,H) :-
   1*D>=0,
   1*A=1,
   1*B=0,
   1*C=0,
   1*E=0,
   1*H=0,
   new14(B,C,D,E,F,G,H).

