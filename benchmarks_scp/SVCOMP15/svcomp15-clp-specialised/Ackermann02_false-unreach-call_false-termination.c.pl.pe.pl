false :-
   main_verifier_error.
ackermann__1(A,B) :-
   true.
ackermann__3(A,B) :-
   1*B=0,
   ackermann__1(A,B).
ackermann__5(A,B) :-
   -1*B>0,
   ackermann__1(A,B).
ackermann__5(A,B) :-
   1*B>0,
   ackermann__1(A,B).
ackermann___0(A,B,C) :-
   1*A+ -1*B=1,
   1*C=0,
   ackermann__3(B,C).
ackermann__8(A,B) :-
   1*A=0,
   ackermann__5(A,B).
ackermann__10(A,B) :-
   -1*A>0,
   ackermann__5(A,B).
ackermann__10(A,B) :-
   1*A>0,
   ackermann__5(A,B).
ackermann___0(A,B,C) :-
   1*A>=2,
   1*B=0,
   1*D=1,
   1*E=0,
   1*F=0,
   1*G=1,
   ackermann__8(B,C),
   ackermann(D,E,F,H,G,A).
ackermann___0(A,B,C) :-
   -1*B+1*K>=0,
   1*A+ -1*K>=1,
   1*D=1,
   1*E=0,
   1*F=0,
   1*B+ -1*G=1,
   1*H=1,
   1*I=0,
   1*J=0,
   ackermann__10(B,C),
   ackermann(D,E,F,C,G,K),
   ackermann(H,I,J,L,K,A).
ackermann__split(A,B,C) :-
   1*A+ -1*B>=1,
   ackermann___0(A,B,C).
ackermann(A,B,C,D,E,F) :-
   -1*E+1*F>=1,
   1*A=1,
   1*B=0,
   1*C=0,
   ackermann__split(F,E,D).
main_entry :-
   true.
main__un :-
   1*E+ -1*F>=1,
   -1*E>= -3,
   1*D>=2,
   1*A=1,
   1*B=0,
   1*C=0,
   main_entry,
   ackermann(A,B,C,D,F,E).
main_verifier_error :-
   main__un.

