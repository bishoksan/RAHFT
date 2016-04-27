false :-
   main_verifier_error.
fibonacci__1(A) :-
   -1*A>= -46.
fibonacci___0(A,B) :-
   -1*B> -1,
   1*A=0,
   fibonacci__1(B).
fibonacci__3(A) :-
   -1*A>= -46,
   1*A>=1,
   fibonacci__1(A).
fibonacci___0(A,B) :-
   1*A=1,
   1*B=1,
   fibonacci__3(B).
fibonacci__5(A) :-
   -1*A>= -46,
   1*A>1,
   fibonacci__3(A).
fibonacci___0(A,B) :-
   1*A+ -1*K>=0,
   1*K>=0,
   -1*B>= -46,
   1*B>1,
   1*C=1,
   1*D=0,
   1*E=0,
   1*B+ -1*F=1,
   1*G=1,
   1*H=0,
   1*I=0,
   1*B+ -1*J=2,
   1*A+ -1*K+ -1*L=0,
   fibonacci__5(B),
   fibonacci(C,D,E,F,K),
   fibonacci(G,H,I,J,L).
fibonacci__split(A,B) :-
   -1*B>= -46,
   1*A>=0,
   fibonacci___0(A,B).
fibonacci(A,B,C,D,E) :-
   -1*D>= -46,
   1*E>=0,
   1*A=1,
   1*B=0,
   1*C=0,
   fibonacci__split(E,D).
main_entry :-
   true.
main__un(A) :-
   -1*A>2147483648,
   main_entry.
main__un(A) :-
   -1*A>= -46,
   1*A> -2147483648,
   main_entry.
main__un1 :-
   -1*E>= -46,
   -1*D+1*E>1,
   1*D>=0,
   1*A=1,
   1*B=0,
   1*C=0,
   main__un(E),
   fibonacci(A,B,C,E,D).
main_verifier_error :-
   main__un1.

