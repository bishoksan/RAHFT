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
addition(A,B,C,D,E,F) :-
   A=1,
   B=1,
   C=1.
addition(A,B,C,D,E,F) :-
   A=0,
   B=1,
   C=1.
addition(A,B,C,D,E,F) :-
   A=0,
   B=0,
   C=0.
addition__1(A,B) :-
   true.
addition___0(A,B,C) :-
   B=A,
   addition__1(B,C),
   C=0.
addition__4(A,B,C) :-
   addition__1(A,B),
   B<0.
addition__4(A,B,C) :-
   addition__1(A,B),
   B>0.
addition__6(A,B) :-
   addition__4(A,B,C),
   B>0.
addition__10(A,B,C) :-
   addition__4(A,B,C),
   B=<0.
addition___0(A,B,C) :-
   D=1,
   E=0,
   F=0,
   G=B+1,
   H=C+ -1,
   addition__6(B,C),
   addition(D,E,F,G,H,A).
addition__12(A,B) :-
   addition__10(A,B,C),
   B<0.
addition___0(A,B,C) :-
   addition__10(B,C,A),
   C>=0.
addition___0(A,B,C) :-
   D=1,
   E=0,
   F=0,
   G=B+ -1,
   H=C+1,
   addition__12(B,C),
   addition(D,E,F,G,H,A).
addition__split(A,B,C) :-
   addition___0(A,B,C).
addition(A,B,C,D,E,F) :-
   A=1,
   B=0,
   C=0,
   addition__split(F,D,E).
main_entry :-
   true.
main__un :-
   main_entry,
   addition(1,0,0,A,B,C),
   C<A-B.
main__un :-
   main_entry,
   addition(1,0,0,A,B,C),
   C>A-B.
main_verifier_error :-
   main__un.

