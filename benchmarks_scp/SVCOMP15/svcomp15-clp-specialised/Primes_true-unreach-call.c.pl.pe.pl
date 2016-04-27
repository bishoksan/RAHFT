false :-
   main_verifier_error.
multiple_of__1(A,B) :-
   -1*A>= -46339,
   1*A>=2.
multiple_of__6(A,B) :-
   -1*A>= -46339,
   1*A>=2,
   multiple_of__1(A,B).
multiple_of__8(A,B) :-
   -1*B>0,
   -1*A>= -46339,
   1*A>=2,
   multiple_of__6(A,B).
multiple_of__11(A,B) :-
   1*B>=0,
   -1*A>= -46339,
   1*A>=2,
   multiple_of__6(A,B).
multiple_of___0(A,B,C) :-
   -1*C>0,
   -1*B>= -46339,
   1*B>=2,
   1*A=1,
   1*D=1,
   1*E=0,
   1*F=0,
   1*C+1*G=0,
   multiple_of__8(B,C),
   multiple_of(D,E,F,G,B,A).
multiple_of__13(A,B) :-
   1*B>=0,
   -1*A>= -46339,
   1*A>=2,
   multiple_of__11(A,B).
multiple_of___0(A,B,C) :-
   -1*B>= -46339,
   1*B>=2,
   1*A=1,
   1*C=0,
   multiple_of__13(B,C).
multiple_of__15(A,B) :-
   1*B>0,
   -1*A>= -46339,
   1*A>=2,
   multiple_of__13(A,B).
multiple_of___0(A,B,C) :-
   1*C>0,
   -1*B>= -46339,
   1*B>=2,
   1*A=1,
   1*D=1,
   1*E=0,
   1*F=0,
   1*B+ -1*C+1*G=0,
   multiple_of__15(B,C),
   multiple_of(D,E,F,G,B,A).
multiple_of__split(A,B,C) :-
   1*B>=2,
   -1*B>= -46339,
   1*B+1*C>0,
   1*A=1,
   multiple_of___0(A,B,C).
multiple_of(A,B,C,D,E,F) :-
   1*E>=2,
   -1*E>= -46339,
   1*D+1*E>0,
   1*A=1,
   1*B=0,
   1*C=0,
   1*F=1,
   multiple_of__split(F,E,D).
is_prime___1(A,B) :-
   -1*B>= -46340,
   -1*A+1*B>=1.
is_prime____0(A,B,C) :-
   -1*C> -2,
   -1*B+1*C>=1,
   1*A=0,
   is_prime___1(B,C).
is_prime___4(A,B,C) :-
   -1*B>= -46340,
   -1*A+1*B>=1,
   1*B>=2,
   is_prime___1(A,B).
is_prime____0(A,B,C) :-
   -1*B>= -1,
   1*A=1,
   1*C=2,
   is_prime___4(B,C,D).
is_prime___6(A,B,C) :-
   -1*B>= -46340,
   -1*A+1*B>=1,
   1*B>2,
   is_prime___4(A,B,C).
is_prime___8(A,B) :-
   -1*B>= -46340,
   -1*A+1*B>=1,
   1*B>2,
   is_prime___6(A,B,C).
is_prime____0(A,B,C) :-
   -1*C>= -46340,
   -1*B+1*C>=1,
   -1*B> -2,
   1*C>2,
   1*A=1,
   is_prime___8(B,C).
is_prime___10(A,B) :-
   -1*B>= -46340,
   -1*A+1*B>=1,
   1*A>=2,
   is_prime___8(A,B).
is_prime___13(A,B) :-
   -1*B>= -46340,
   -1*A+1*B>=1,
   1*A>=2,
   1*C=1,
   1*D=0,
   1*E=0,
   1*F=1,
   is_prime___10(A,B),
   multiple_of(C,D,E,B,A,F).
is_prime____0(A,B,C) :-
   46338*A+ -1*C>= -2,
   1*B>=2,
   -1*B+1*C>=1,
   -1*A>= -1,
   1*D=1,
   1*E=0,
   1*F=0,
   1*B+ -1*G=1,
   is_prime___13(B,C),
   is_prime_(D,E,F,C,G,A).
is_prime___split(A,B,C) :-
   46338*A+ -1*C>= -2,
   1*A>=0,
   1*B+ -1*C>= -46339,
   -1*B+1*C>=1,
   -1*A>= -1,
   46339*A+ -1*C> -2,
   is_prime____0(A,B,C).
is_prime_(A,B,C,D,E,F) :-
   1*F>=0,
   -1*F>= -1,
   -1*D+1*E>= -46339,
   -1*D+46339*F> -2,
   -1*D+46338*F>= -2,
   1*D+ -1*E>=1,
   1*A=1,
   1*B=0,
   1*C=0,
   is_prime___split(F,E,D).
is_prime__1(A) :-
   -1*A>= -46340.
is_prime__split(A,B) :-
   1*A>=0,
   46338*A+ -1*B>= -2,
   -1*A>= -1,
   46339*A+ -1*B> -2,
   1*C=1,
   1*D=0,
   1*E=0,
   1*B+ -1*F=1,
   is_prime__1(B),
   is_prime_(C,D,E,B,F,A).
is_prime(A,B,C,D,E) :-
   -1*E>= -1,
   -1*D+46339*E> -2,
   -1*D+46338*E>= -2,
   1*E>=0,
   1*A=1,
   1*B=0,
   1*C=0,
   is_prime__split(E,D).
mult__1(A,B) :-
   -1*B>= -46340.
mult__3(A,B) :-
   -1*B>= -46340,
   -1*A>0,
   mult__1(A,B).
mult__br2(A,B) :-
   -1*B>= -46340,
   1*A>=0,
   mult__1(A,B).
mult___0(A,B,C) :-
   -1*C>= -46340,
   -1*B>0,
   1*D=1,
   1*E=0,
   1*F=0,
   1*B+1*G=0,
   mult__3(B,C),
   mult(D,E,F,C,G,A).
mult_NodeBlock(A,B) :-
   -1*B>= -46340,
   1*A>=0,
   mult__br2(A,B).
mult_LeafBlock(A,B) :-
   -1*B>= -46340,
   -1*A> -1,
   1*A>=0,
   mult_NodeBlock(A,B).
mult_LeafBlock1(A,B) :-
   -1*B>= -46340,
   1*A>=1,
   mult_NodeBlock(A,B).
mult__br3(A,B) :-
   -1*B>= -46340,
   1*A=1,
   mult_LeafBlock1(A,B).
mult_NewDefault(A,B) :-
   -1*B>= -46340,
   1*A>1,
   mult_LeafBlock1(A,B).
mult___0(A,B,C) :-
   -1*C>= -46340,
   1*A=0,
   1*B=0,
   mult_LeafBlock(B,C).
mult_NewDefault(A,B) :-
   -1*B>= -46340,
   -1*A> -1,
   1*A>0,
   mult_LeafBlock(A,B).
mult___0(A,B,C) :-
   -1*C>= -46340,
   1*A=1,
   1*B=1,
   mult__br3(B,C).
mult__8(A,B) :-
   -1*B>= -46340,
   1*A>0,
   mult_NewDefault(A,B).
mult___0(A,B,C) :-
   -1*C>= -46340,
   1*B>0,
   1*D=1,
   1*E=0,
   1*F=0,
   1*B+ -1*G=1,
   1*A+ -1*C+ -1*H=0,
   mult__8(B,C),
   mult(D,E,F,C,G,H).
mult__split(A,B,C) :-
   -1*C>= -46340,
   mult___0(A,B,C).
mult(A,B,C,D,E,F) :-
   -1*D>= -46340,
   1*A=1,
   1*B=0,
   1*C=0,
   mult__split(F,E,D).
main_entry :-
   true.
main__un(A) :-
   -1*A>= -46340,
   main_entry.
main__un1(A,B,C) :-
   -1*C>= -1,
   -1*B+46339*C> -2,
   -1*B+46338*C>= -2,
   -1*A>= -46340,
   1*C>=0,
   1*D=1,
   1*E=0,
   1*F=0,
   main__un(B),
   is_prime(D,E,F,B,C).
main__un2(A,B,C) :-
   -1*C>= -46340,
   -1*A>= -46340,
   1*D=1,
   main__un1(A,C,D).
main__un3 :-
   1*G>1,
   -1*F>= -46340,
   -1*D>= -46340,
   1*F>1,
   1*A=1,
   1*B=0,
   1*C=0,
   1*D+ -1*E=0,
   main__un2(F,G,E),
   mult(A,B,C,F,G,D).
main_verifier_error :-
   main__un3.

