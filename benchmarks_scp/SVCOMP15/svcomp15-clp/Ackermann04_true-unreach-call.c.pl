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
ackermann(A,B,C,D,E,F) :-
   A=1,
   B=1,
   C=1.
ackermann(A,B,C,D,E,F) :-
   A=0,
   B=1,
   C=1.
ackermann(A,B,C,D,E,F) :-
   A=0,
   B=0,
   C=0.
ackermann__1(A,B) :-
   true.
ackermann__3(A,B) :-
   ackermann__1(A,B),
   B=0.
ackermann__5(A,B) :-
   ackermann__1(A,B),
   B<0.
ackermann__5(A,B) :-
   ackermann__1(A,B),
   B>0.
ackermann___0(A,B,C) :-
   ackermann__3(B,C),
   A=B+1.
ackermann__8(A,B) :-
   ackermann__5(A,B),
   A=0.
ackermann__10(A,B) :-
   ackermann__5(A,B),
   A<0.
ackermann__10(A,B) :-
   ackermann__5(A,B),
   A>0.
ackermann___0(A,B,C) :-
   D=1,
   E=0,
   F=0,
   G=1,
   ackermann__8(B,C),
   ackermann(D,E,F,H,G,A).
ackermann___0(A,B,C) :-
   D=1,
   E=0,
   F=0,
   ackermann__10(B,C),
   ackermann(1,0,0,C,B+ -1,G),
   ackermann(D,E,F,H,G,A).
ackermann__split(A,B,C) :-
   ackermann___0(A,B,C).
ackermann(A,B,C,D,E,F) :-
   A=1,
   B=0,
   C=0,
   ackermann__split(F,E,D).
main_entry :-
   true.
main__un(A) :-
   main_entry,
   A=<3.
main__un1(A,B) :-
   main__un(A),
   B=<23.
main__un2 :-
   main__un1(A,B),
   ackermann(1,0,0,A,B,C),
   C=<6,
   A>=2,
   B>=2.
main_verifier_error :-
   main__un2.

