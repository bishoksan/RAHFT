hanoi__1(A) :-
   -1*A>= -31.
hanoi___0(A,B) :-
   1*A=1,
   1*B=1,
   hanoi__1(B).
hanoi__3(A) :-
   -1*A> -1,
   hanoi__1(A).
hanoi__3(A) :-
   -1*A>= -31,
   1*A>1,
   hanoi__1(A).
hanoi___0(A,B) :-
   1*A+ -4*B>= -5,
   1*A+ -8*B>= -17,
   -1*B>= -31,
   1*B>=2,
   1*C=1,
   1*D=0,
   1*E=0,
   1*B+ -1*F=1,
   1*A+ -2*G=1,
   hanoi__3(B),
   hanoi(C,D,E,F,G).
hanoi__split(A,B) :-
   1*A+ -4*B>= -5,
   1*A+ -8*B>= -17,
   1*B>=1,
   -1*B>= -31,
   1*A+ -2*B>= -1,
   hanoi___0(A,B).
hanoi(A,B,C,D,E) :-
   -1*D>= -31,
   -2*D+1*E>= -1,
   -4*D+1*E>= -5,
   -8*D+1*E>= -17,
   1*D>=1,
   1*A=1,
   1*B=0,
   1*C=0,
   hanoi__split(E,D).
main_entry :-
   true.
main__un(A) :-
   -1*A>= -31,
   main_entry.

