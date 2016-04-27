false :-
   main_verifier_error.
fibo__1(A) :-
   -1*A>= -5,
   1*A> -1.
fibo___0(A,B) :-
   -1*B> -1,
   1*B> -1,
   1*A=0,
   fibo__1(B).
fibo__3(A) :-
   -1*A>= -5,
   1*A>=1,
   fibo__1(A).
fibo___0(A,B) :-
   1*A=1,
   1*B=1,
   fibo__3(B).
fibo__5(A) :-
   -1*A>= -5,
   1*A>1,
   fibo__3(A).
fibo___0(A,B) :-
   1*K>=0,
   1*A+ -1*K>=0,
   -1*B>= -5,
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
   fibo__5(B),
   fibo(C,D,E,F,K),
   fibo(G,H,I,J,L).
fibo__split(A,B) :-
   1*B> -1,
   -1*B>= -5,
   1*A>=0,
   fibo___0(A,B).
fibo(A,B,C,D,E) :-
   1*E>=0,
   -1*D>= -5,
   1*D> -1,
   1*A=1,
   1*B=0,
   1*C=0,
   fibo__split(E,D).
main_entry :-
   true.
main__un :-
   -1*E> -5,
   1*E>=0,
   1*A=1,
   1*B=0,
   1*C=0,
   1*D=5,
   main_entry,
   fibo(A,B,C,D,E).
main__un :-
   1*E>5,
   1*A=1,
   1*B=0,
   1*C=0,
   1*D=5,
   main_entry,
   fibo(A,B,C,D,E).
main_verifier_error :-
   main__un.

