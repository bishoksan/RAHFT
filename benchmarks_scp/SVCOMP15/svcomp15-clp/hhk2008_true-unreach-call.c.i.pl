false :-
   main__split.
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
main__1 :-
   true.
main__cnt_0(A,B,C,D) :-
   main__1,
   E<1000001,
   F> -1,
   G<1000001.
main__14(A,B,C,D) :-
   main__cnt_0(A,B,C,D),
   A>0.
main__17(A,B,C) :-
   main__cnt_0(D,A,B,C),
   D=<0.
main__cnt_0(A,B,C,D) :-
   main__14(E,F,C,D),
   A=E+ -1,
   B=F+1.
main__split :-
   main__17(A,B,C),
   A=B+C,
   D=1.
main__split :-
   main__17(A,B,C),
   A<B+C,
   D=0.
main__split :-
   main__17(A,B,C),
   A>B+C,
   D=0.

