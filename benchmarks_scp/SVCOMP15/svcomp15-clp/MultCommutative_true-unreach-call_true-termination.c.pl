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
mult(A,B,C,D,E,F) :-
   A=1,
   B=1,
   C=1.
mult(A,B,C,D,E,F) :-
   A=0,
   B=1,
   C=1.
mult(A,B,C,D,E,F) :-
   A=0,
   B=0,
   C=0.
mult__1(A,B) :-
   true.
mult__3(A,B) :-
   mult__1(A,B),
   A<0.
mult__6(A,B) :-
   mult__1(A,B),
   A>=0.
mult___0(A,B,C) :-
   D=1,
   E=0,
   F=0,
   G=0-B,
   mult__3(B,C),
   mult(D,E,F,C,G,A).
mult___0(A,B,C) :-
   mult__6(B,C),
   B=0,
   A=0.
mult__8(A,B) :-
   mult__6(A,B),
   A<0.
mult__8(A,B) :-
   mult__6(A,B),
   A>0.
mult___0(A,B,C) :-
   mult__8(B,C),
   mult(1,0,0,C,B+ -1,D),
   A=D+C.
mult__split(A,B,C) :-
   mult___0(A,B,C).
mult(A,B,C,D,E,F) :-
   A=1,
   B=0,
   C=0,
   mult__split(F,E,D).
main_entry :-
   true.
main__un(A) :-
   main_entry,
   A=<46340.
main__un1(A,B) :-
   main__un(A),
   B=<46340.
main__un2 :-
   main__un1(A,B),
   mult(1,0,0,A,B,C),
   mult(1,0,0,B,A,D),
   C<D,
   A>0,
   B>0.
main__un2 :-
   main__un1(A,B),
   mult(1,0,0,A,B,C),
   mult(1,0,0,B,A,D),
   C>D,
   A>0,
   B>0.
main_verifier_error :-
   main__un2.

