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
hanoi(A,B,C,D,E) :-
   A=1,
   B=1,
   C=1.
hanoi(A,B,C,D,E) :-
   A=0,
   B=1,
   C=1.
hanoi(A,B,C,D,E) :-
   A=0,
   B=0,
   C=0.
hanoi__1(A) :-
   true.
hanoi___0(A,B) :-
   hanoi__1(B),
   B=1,
   A=1.
hanoi__3(A) :-
   hanoi__1(A),
   A<1.
hanoi__3(A) :-
   hanoi__1(A),
   A>1.
hanoi___0(A,B) :-
   hanoi__3(B),
   hanoi(1,0,0,B+ -1,C),
   A=C*2+1.
hanoi__split(A,B) :-
   hanoi___0(A,B).
hanoi(A,B,C,D,E) :-
   A=1,
   B=0,
   C=0,
   hanoi__split(E,D).
main_entry :-
   true.
main__un(A) :-
   main_entry,
   A+ -1=<30.
main__un1 :-
   main__un(A),
   hanoi(1,0,0,A,B),
   B<A.
main_verifier_error :-
   main__un1.

