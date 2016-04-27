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
isOdd(A,B,C,D,E) :-
   A=1,
   B=1,
   C=1.
isOdd(A,B,C,D,E) :-
   A=0,
   B=1,
   C=1.
isOdd(A,B,C,D,E) :-
   A=0,
   B=0,
   C=0.
isOdd__br(A) :-
   true.
isOdd_NodeBlock(A) :-
   isOdd__br(A).
isOdd_LeafBlock(A) :-
   isOdd_NodeBlock(A),
   A<1.
isOdd_LeafBlock1(A) :-
   isOdd_NodeBlock(A),
   A>=1.
isOdd__br1(A) :-
   isOdd_LeafBlock1(A),
   A=1.
isOdd_NewDefault(A) :-
   isOdd_LeafBlock1(A),
   A<1.
isOdd_NewDefault(A) :-
   isOdd_LeafBlock1(A),
   A>1.
isOdd___0(A,B) :-
   isOdd_LeafBlock(B),
   B=0,
   A=0.
isOdd_NewDefault(A) :-
   isOdd_LeafBlock(A),
   A<0.
isOdd_NewDefault(A) :-
   isOdd_LeafBlock(A),
   A>0.
isOdd___0(A,B) :-
   isOdd__br1(B),
   A=1.
isOdd__3(A) :-
   isOdd_NewDefault(A).
isOdd___0(A,B) :-
   isOdd__3(B).
isOdd__split(A,B) :-
   isOdd___0(A,B).
isOdd(A,B,C,D,E) :-
   A=1,
   B=0,
   C=0,
   isOdd__split(E,D).
main_entry :-
   true.
main__un :-
   main_entry,
   A>=0,
   A<B.
main__un :-
   main_entry,
   A>=0,
   A>B.
main_verifier_error :-
   main__un.

