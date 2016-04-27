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
f91(A,B,C,D,E) :-
   A=1,
   B=1,
   C=1.
f91(A,B,C,D,E) :-
   A=0,
   B=1,
   C=1.
f91(A,B,C,D,E) :-
   A=0,
   B=0,
   C=0.
f91__1(A) :-
   true.
f91__3(A) :-
   f91__1(A),
   A>100.
f91__5(A) :-
   f91__1(A),
   A=<100.
f91___0(A,B) :-
   f91__3(B),
   A=B+ -10.
f91___0(A,B) :-
   C=1,
   D=0,
   E=0,
   f91__5(B),
   f91(1,0,0,B+11,F),
   f91(C,D,E,F,A).
f91__split(A,B) :-
   f91___0(A,B).
f91(A,B,C,D,E) :-
   A=1,
   B=0,
   C=0,
   f91__split(E,D).
main_entry :-
   true.
main__un(A,B) :-
   main_entry,
   f91(1,0,0,A,B),
   B<91.
main__un(A,B) :-
   main_entry,
   f91(1,0,0,A,B),
   B>91.
main__un1 :-
   main__un(A,B),
   A=<102.
main__un1 :-
   main__un(A,B),
   B<A+ -10.
main__un1 :-
   main__un(A,B),
   B>A+ -10.
main_verifier_error :-
   main__un1.

