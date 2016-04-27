new20(A,B,C,D,E,F,G,H) :-
   1*C>=0,
   1*D>=0,
   1*E>=0,
   1*B>=0,
   1*A=0,
   1*H=0.
new18(A,B,C,D,E,F,G) :-
   1*A=0,
   1*B=0,
   1*C=0,
   1*D=0,
   1*G=0,
   1*H=0,
   new20(H,A,B,C,D,E,F,G).
new16(A,B,C,D,E,F,G,H) :-
   1*A=1,
   1*B=0,
   1*C=0,
   1*D=0,
   1*E=0,
   1*H=0,
   new18(B,C,D,E,F,G,H).
new14(A,B,C,D,E,F,G) :-
   1*A=0,
   1*B=0,
   1*C=0,
   1*D=0,
   1*G=0,
   1*H=1,
   new16(H,A,B,C,D,E,F,G).
new12(A,B,C,D,E,F,G,H) :-
   1*A=1,
   1*B=0,
   1*C=0,
   1*D=0,
   1*E=0,
   1*H=0,
   new14(B,C,D,E,F,G,H).

