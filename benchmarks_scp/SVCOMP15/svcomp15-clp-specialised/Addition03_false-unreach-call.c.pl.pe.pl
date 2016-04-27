false :-
   main_verifier_error.
addition__1(A,B) :-
   true.
addition___0(A,B,C) :-
   1*A+ -1*B=0,
   1*C=0,
   addition__1(B,C).
addition__4(A,B,C) :-
   -1*B>0,
   addition__1(A,B).
addition__4(A,B,C) :-
   1*B>0,
   addition__1(A,B).
addition__6(A,B) :-
   1*B>0,
   addition__4(A,B,C).
addition__10(A,B,C) :-
   -1*B>=0,
   addition__4(A,B,C).
addition___0(A,B,C) :-
   1*C>0,
   1*D=1,
   1*E=0,
   1*F=0,
   1*B+ -1*G= -1,
   1*C+ -1*H=1,
   addition__6(B,C),
   addition(D,E,F,G,H,A).
addition__12(A,B) :-
   -1*B>0,
   addition__10(A,B,C).
addition___0(A,B,C) :-
   1*C=0,
   addition__10(B,C,A).
addition___0(A,B,C) :-
   -1*C>0,
   1*D=1,
   1*E=0,
   1*F=0,
   1*B+ -1*G=1,
   1*C+ -1*H= -1,
   addition__12(B,C),
   addition(D,E,F,G,H,A).
addition__split(A,B,C) :-
   addition___0(A,B,C).
addition(A,B,C,D,E,F) :-
   1*A=1,
   1*B=0,
   1*C=0,
   addition__split(F,D,E).
main_entry :-
   true.
main__un :-
   1*F>=100,
   -1*D>= -199,
   1*E>=100,
   1*A=1,
   1*B=0,
   1*C=0,
   main_entry,
   addition(A,B,C,E,F,D).
main_verifier_error :-
   main__un.

