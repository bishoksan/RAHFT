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
main__br :-
   true.
main__w_0(A,B,C,D) :-
   main__br,
   A=0,
   B=0,
   C=0,
   D=0.
main__5(A,B,C,D) :-
   main__w_0(A,B,C,D),
   C<10000,
   E<0.
main__5(A,B,C,D) :-
   main__w_0(A,B,C,D),
   C<10000,
   E>0.
main__28(A,B) :-
   main__w_0(C,D,A,B),
   A>=10000.
main__28(A,B) :-
   main__w_0(C,D,A,B),
   E=0.
main__11(A,B,C,D) :-
   main__5(A,B,C,D),
   E=0.
main__8(A,B,C,D) :-
   main__5(A,B,C,D),
   E<0.
main__8(A,B,C,D) :-
   main__5(A,B,C,D),
   E>0.
main__y_4(A,B,C,D) :-
   main__8(A,B,E,F),
   C=E+100,
   D=F+1.
main__19(A,B,C,D) :-
   main__11(A,B,C,D),
   E=0.
main__14(A,B,C,D) :-
   main__11(A,B,C,D),
   E<0.
main__14(A,B,C,D) :-
   main__11(A,B,C,D),
   E>0.
main__16(A,B,C,D) :-
   main__14(A,B,C,D),
   D>3.
main__y_4(A,B,C,D) :-
   main__14(A,B,C,D),
   D=<3.
main__y_4(A,B,C,D) :-
   main__16(A,B,E,F),
   D=F+1,
   C=E+1.
main__y_4(A,B,C,D) :-
   main__19(A,B,E,D),
   E=<A*10,
   C=E.
main__y_4(A,B,C,D) :-
   main__19(A,B,E,D),
   B<D*100,
   C=E.
main__y_4(A,B,C,D) :-
   main__19(A,B,E,D),
   E>A*10,
   B>=D*100,
   C=0-E.
main__w_0(A,B,C,D) :-
   main__y_4(E,F,C,D),
   A=E+1,
   B=F+10.
main__split :-
   main__28(A,B),
   B=<3,
   C=0.
main__split :-
   main__28(A,B),
   A>=3,
   C=0.
main__split :-
   main__28(A,B),
   B>3,
   A<3,
   C=1.

