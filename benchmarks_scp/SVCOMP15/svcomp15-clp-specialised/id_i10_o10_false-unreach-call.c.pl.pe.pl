false :-
   main_verifier_error.
id__1(A) :-
   -1*A>= -10.
id___0(A,B) :-
   1*A=0,
   1*B=0,
   id__1(B).
id__3(A) :-
   -1*A>0,
   id__1(A).
id__3(A) :-
   -1*A>= -10,
   1*A>0,
   id__1(A).
id___0(A,B) :-
   -1*A>= -10,
   1*A>=1,
   1*A+ -1*B=0,
   1*C=1,
   1*D=0,
   1*E=0,
   1*A+ -1*F=1,
   1*A+ -1*G=1,
   id__3(B),
   id(C,D,E,F,G).
id__split(A,B) :-
   -1*A>= -10,
   1*A>=0,
   1*A+ -1*B=0,
   id___0(A,B).
id(A,B,C,D,E) :-
   -1*D>= -10,
   1*D>=0,
   1*A=1,
   1*B=0,
   1*C=0,
   1*D+ -1*E=0,
   id__split(E,D).
main_entry :-
   true.
main__un :-
   1*A=1,
   1*B=0,
   1*C=0,
   1*D=10,
   1*E=10,
   main_entry,
   id(A,B,C,D,E).
main_verifier_error :-
   main__un.

