false :-
   main_verifier_error.
sum__1(A,B) :-
   -1*A>= -5,
   1*A>=3,
   1*A+1*B=5.
sum__3(A,B) :-
   1*A>4,
   -1*A>= -5,
   1*A+1*B=5,
   sum__1(A,B).
sum__5(A,B) :-
   -1*A>= -4,
   1*A>=3,
   1*A+1*B=5,
   sum__1(A,B).
sum___0(A,B,C) :-
   -1*B>= -5,
   1*B>4,
   1*A=5,
   1*B+1*C=5,
   sum__3(B,C).
sum___0(A,B,C) :-
   1*B>=3,
   -1*B>= -4,
   1*A=5,
   1*B+1*C=5,
   1*D=1,
   1*E=0,
   1*F=0,
   1*B+1*G=4,
   1*B+ -1*H= -1,
   sum__5(B,C),
   sum(D,E,F,G,H,A).
sum__split(A,B,C) :-
   1*B>=3,
   -1*B>= -5,
   1*A=5,
   1*B+1*C=5,
   sum___0(A,B,C).
sum(A,B,C,D,E,F) :-
   1*D>=0,
   -1*D>= -2,
   1*A=1,
   1*B=0,
   1*C=0,
   1*D+1*E=5,
   1*F=5,
   sum__split(F,E,D).
main_entry :-
   true.
main__un :-
   1*A=1,
   1*B=0,
   1*C=0,
   1*D=2,
   1*E=3,
   1*F=5,
   main_entry,
   sum(A,B,C,D,E,F).
main_verifier_error :-
   main__un.

