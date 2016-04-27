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
sum(A,B,C,D,E,F) :-
   A=1,
   B=1,
   C=1.
sum(A,B,C,D,E,F) :-
   A=0,
   B=1,
   C=1.
sum(A,B,C,D,E,F) :-
   A=0,
   B=0,
   C=0.
sum__1(A,B) :-
   true.
sum__3(A,B) :-
   sum__1(A,B),
   B<1.
sum__5(A,B) :-
   sum__1(A,B),
   B>=1.
sum___0(A,B,C) :-
   sum__3(B,C),
   A=B+C.
sum___0(A,B,C) :-
   D=1,
   E=0,
   F=0,
   G=C+ -1,
   H=B+1,
   sum__5(B,C),
   sum(D,E,F,G,H,A).
sum__split(A,B,C) :-
   sum___0(A,B,C).
sum(A,B,C,D,E,F) :-
   A=1,
   B=0,
   C=0,
   sum__split(F,E,D).
main_entry :-
   true.
main__un :-
   main_entry,
   sum(1,0,0,2,3,A),
   A<5.
main__un :-
   main_entry,
   sum(1,0,0,2,3,A),
   A>5.
main_verifier_error :-
   main__un.

