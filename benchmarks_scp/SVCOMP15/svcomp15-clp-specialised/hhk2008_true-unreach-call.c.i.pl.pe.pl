false :-
   main__split.
main__1 :-
   true.
main__cnt_0(A,B,C,D) :-
   -1*G> -1000001,
   -1*E> -1000001,
   1*F> -1,
   main__1.
main__14(A,B,C,D) :-
   1*A>0,
   main__cnt_0(A,B,C,D).
main__17(A,B,C) :-
   -1*D>=0,
   main__cnt_0(D,A,B,C).
main__cnt_0(A,B,C,D) :-
   1*A> -1,
   1*A+ -1*E= -1,
   1*B+ -1*F=1,
   main__14(E,F,C,D).
main__split :-
   1*A+ -1*B+ -1*C=0,
   1*D=1,
   main__17(A,B,C).
main__split :-
   -1*A+1*B+1*C>0,
   1*D=0,
   main__17(A,B,C).
main__split :-
   1*A+ -1*B+ -1*C>0,
   1*D=0,
   main__17(A,B,C).

