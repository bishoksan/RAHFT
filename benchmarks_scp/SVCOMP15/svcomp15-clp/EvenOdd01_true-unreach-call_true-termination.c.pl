false :-
   main_verifier_error.
verifier_error(A,B,C) :-
   A=0,
   B=0,
   C=0.
verifier_error(A,B,C) :-
   A=0,
   B=1,
   C=1.
verifier_error(A,B,C) :-
   A=1,
   B=0,
   C=1.
verifier_error(A,B,C) :-
   A=1,
   B=1,
   C=1.
isEven(A,B,C,D,E) :-
   A=1,
   B=1,
   C=1.
isEven(A,B,C,D,E) :-
   A=0,
   B=1,
   C=1.
isEven(A,B,C,D,E) :-
   A=0,
   B=0,
   C=0.
isEven__br(A) :-
   true.
isEven_NodeBlock(A) :-
   isEven__br(A).
isEven_LeafBlock(A) :-
   isEven_NodeBlock(A),
   A<1.
isEven_LeafBlock1(A) :-
   isEven_NodeBlock(A),
   A>=1.
isEven__br1(A) :-
   isEven_LeafBlock1(A),
   A=1.
isEven_NewDefault(A) :-
   isEven_LeafBlock1(A),
   A<1.
isEven_NewDefault(A) :-
   isEven_LeafBlock1(A),
   A>1.
isEven___0(A,B) :-
   isEven_LeafBlock(B),
   B=0,
   A=1.
isEven_NewDefault(A) :-
   isEven_LeafBlock(A),
   A<0.
isEven_NewDefault(A) :-
   isEven_LeafBlock(A),
   A>0.
isEven___0(A,B) :-
   isEven__br1(B),
   A=0.
isEven__3(A) :-
   isEven_NewDefault(A).
isEven___0(A,B) :-
   isEven__3(B).
isEven__split(A,B) :-
   isEven___0(A,B).
isEven(A,B,C,D,E) :-
   A=1,
   B=0,
   C=0,
   isEven__split(E,D).
main_entry :-
   true.
main__un :-
   main_entry,
   A>=0.
main__un1 :-
   main__un,
   A>=0,
   A<B.
main__un1 :-
   main__un,
   A>=0,
   A>B.
main_verifier_error :-
   main__un1.

