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
main__a_0(A,B,C,D) :-
   main__1,
   D> -1,
   A=0,
   B=0,
   C=0.
main__6(A,B,C,D) :-
   main__a_0(A,B,C,D),
   A<D.
main__17(A,B,C) :-
   main__a_0(D,A,B,C),
   D>=C.
main__12(A,B,C,D) :-
   main__6(A,B,C,D),
   E=0.
main__9(A,B,C,D) :-
   main__6(A,B,C,D),
   E<0.
main__9(A,B,C,D) :-
   main__6(A,B,C,D),
   E>0.
main__a_1(A,B,C,D) :-
   main__9(A,E,F,D),
   B=E+1,
   C=F+2.
main__a_1(A,B,C,D) :-
   main__12(A,E,F,D),
   B=E+2,
   C=F+1.
main__a_0(A,B,C,D) :-
   main__a_1(E,B,C,D),
   A=E+1.
main__split :-
   main__17(A,B,C),
   A+B=C*3,
   D=1.
main__split :-
   main__17(A,B,C),
   A+B<C*3,
   D=0.
main__split :-
   main__17(A,B,C),
   A+B>C*3,
   D=0.

