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
sum___0(A,B,C) :-
   C=A,
   sum__1(B,C),
   B=0.
sum__3(A,B) :-
   sum__1(A,B),
   A<0.
sum__3(A,B) :-
   sum__1(A,B),
   A>0.
sum___0(A,B,C) :-
   D=1,
   E=0,
   F=0,
   G=B+ -1,
   H=C+1,
   sum__3(B,C),
   sum(D,E,F,G,H,A).
sum__split(A,B,C) :-
   sum___0(A,B,C).
sum(A,B,C,D,E,F) :-
   A=1,
   B=0,
   C=0,
   sum__split(F,D,E).
main_entry :-
   true.
main__un :-
   main_entry,
   sum(1,0,0,A,B,C),
   C<D+E.
main__un :-
   main_entry,
   sum(1,0,0,A,B,C),
   C>D+E.
main_verifier_error :-
   main__un.

