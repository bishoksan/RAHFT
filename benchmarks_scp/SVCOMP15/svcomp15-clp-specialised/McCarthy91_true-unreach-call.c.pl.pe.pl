false :-
   main_verifier_error.
f91__1(A) :-
   true.
f91__3(A) :-
   1*A>100,
   f91__1(A).
f91__5(A) :-
   -1*A>= -100,
   f91__1(A).
f91___0(A,B) :-
   1*A>90,
   1*A+ -1*B= -10,
   f91__3(B).
f91___0(A,B) :-
   1*A+ -1*J>= -10,
   1*J>90,
   -1*B+1*J>=1,
   -1*B>= -100,
   1*A>90,
   1*C=1,
   1*D=0,
   1*E=0,
   1*B+ -1*F= -11,
   1*G=1,
   1*H=0,
   1*I=0,
   f91__5(B),
   f91(C,D,E,F,J),
   f91(G,H,I,J,A).
f91__split(A,B) :-
   1*A+ -1*B>= -10,
   1*A>90,
   f91___0(A,B).
f91(A,B,C,D,E) :-
   -1*D+1*E>= -10,
   1*E>90,
   1*A=1,
   1*B=0,
   1*C=0,
   f91__split(E,D).
main_entry :-
   true.
main__un(A,B) :-
   -1*B> -91,
   -1*A+1*B>= -10,
   1*B>90,
   1*C=1,
   1*D=0,
   1*E=0,
   main_entry,
   f91(C,D,E,A,B).
main__un(A,B) :-
   -1*A+1*B>= -10,
   1*B>91,
   1*C=1,
   1*D=0,
   1*E=0,
   main_entry,
   f91(C,D,E,A,B).
main__un1 :-
   -1*A+1*B>= -10,
   -1*A>= -101,
   1*B>90,
   main__un(A,B).
main__un1 :-
   1*A+ -1*B> -10,
   1*A>90,
   main__un(B,A).
main_verifier_error :-
   main__un1.

