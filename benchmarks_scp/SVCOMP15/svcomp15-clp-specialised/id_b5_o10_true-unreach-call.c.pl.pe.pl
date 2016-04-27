id__1(A) :-
   true.
id__br1(A) :-
   1*A=0,
   id__1(A).
id__3(A) :-
   -1*A>0,
   id__1(A).
id__3(A) :-
   1*A>0,
   id__1(A).
id_UnifiedReturnBlock(A,B) :-
   -1*G>= -5,
   1*G>4,
   1*B+ -1*G>=1,
   1*A=5,
   1*C=1,
   1*D=0,
   1*E=0,
   1*B+ -1*F=1,
   id__3(B),
   id(C,D,E,F,G).
id_UnifiedReturnBlock(A,B) :-
   -1*A+1*B>=0,
   -1*A>= -5,
   1*A>=1,
   1*C=1,
   1*D=0,
   1*E=0,
   1*B+ -1*F=1,
   1*A+ -1*G=1,
   id__3(B),
   id(C,D,E,F,G).
id_UnifiedReturnBlock(A,B) :-
   1*A=0,
   1*B=0,
   id__br1(B).
id_UnifiedReturnBlock_split(A,B) :-
   -1*A+1*B>=0,
   -1*A>= -5,
   1*A>=0,
   id_UnifiedReturnBlock(A,B).
id(A,B,C,D,E) :-
   1*E>=0,
   -1*E>= -5,
   1*D+ -1*E>=0,
   1*A=1,
   1*B=0,
   1*C=0,
   id_UnifiedReturnBlock_split(E,D).
main_entry :-
   true.

