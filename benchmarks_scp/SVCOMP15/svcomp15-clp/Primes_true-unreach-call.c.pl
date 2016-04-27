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
multiple_of(A,B,C,D,E,F) :-
   A=1,
   B=1,
   C=1.
multiple_of(A,B,C,D,E,F) :-
   A=0,
   B=1,
   C=1.
multiple_of(A,B,C,D,E,F) :-
   A=0,
   B=0,
   C=0.
multiple_of__1(A,B) :-
   true.
multiple_of__3(A,B) :-
   multiple_of__1(A,B),
   A<0.
multiple_of__6(A,B) :-
   multiple_of__1(A,B),
   A>=0.
multiple_of___0(A,B,C) :-
   D=1,
   E=0,
   F=0,
   G=0-B,
   multiple_of__3(B,C),
   multiple_of(D,E,F,C,G,A).
multiple_of__8(A,B) :-
   multiple_of__6(A,B),
   B<0.
multiple_of__11(A,B) :-
   multiple_of__6(A,B),
   B>=0.
multiple_of___0(A,B,C) :-
   D=1,
   E=0,
   F=0,
   G=0-C,
   multiple_of__8(B,C),
   multiple_of(D,E,F,G,B,A).
multiple_of___0(A,B,C) :-
   multiple_of__11(B,C),
   B=0,
   A=0.
multiple_of__13(A,B) :-
   multiple_of__11(A,B),
   A<0.
multiple_of__13(A,B) :-
   multiple_of__11(A,B),
   A>0.
multiple_of___0(A,B,C) :-
   multiple_of__13(B,C),
   C=0,
   A=1.
multiple_of__15(A,B) :-
   multiple_of__13(A,B),
   B<0.
multiple_of__15(A,B) :-
   multiple_of__13(A,B),
   B>0.
multiple_of___0(A,B,C) :-
   D=1,
   E=0,
   F=0,
   G=C-B,
   multiple_of__15(B,C),
   multiple_of(D,E,F,G,B,A).
multiple_of__split(A,B,C) :-
   multiple_of___0(A,B,C).
multiple_of(A,B,C,D,E,F) :-
   A=1,
   B=0,
   C=0,
   multiple_of__split(F,E,D).
is_prime_(A,B,C,D,E,F) :-
   A=1,
   B=1,
   C=1.
is_prime_(A,B,C,D,E,F) :-
   A=0,
   B=1,
   C=1.
is_prime_(A,B,C,D,E,F) :-
   A=0,
   B=0,
   C=0.
is_prime___1(A,B) :-
   true.
is_prime____0(A,B,C) :-
   is_prime___1(B,C),
   C<2,
   A=0.
is_prime___4(A,B,C) :-
   is_prime___1(A,B),
   B>=2.
is_prime____0(A,B,C) :-
   is_prime___4(B,C,D),
   C=2,
   A=1.
is_prime___6(A,B,C) :-
   is_prime___4(A,B,C),
   B<2.
is_prime___6(A,B,C) :-
   is_prime___4(A,B,C),
   B>2.
is_prime___8(A,B) :-
   is_prime___6(A,B,C),
   B>2.
is_prime____0(A,B,C) :-
   is_prime___6(B,C,A),
   C=<2.
is_prime____0(A,B,C) :-
   is_prime___8(B,C),
   B<2,
   A=1.
is_prime___10(A,B) :-
   is_prime___8(A,B),
   A>=2.
is_prime____0(A,B,C) :-
   is_prime___10(B,C),
   multiple_of(1,0,0,C,B,D),
   D=0,
   A=0.
is_prime___13(A,B) :-
   is_prime___10(A,B),
   multiple_of(1,0,0,B,A,C),
   C<0.
is_prime___13(A,B) :-
   is_prime___10(A,B),
   multiple_of(1,0,0,B,A,C),
   C>0.
is_prime____0(A,B,C) :-
   D=1,
   E=0,
   F=0,
   G=B+ -1,
   is_prime___13(B,C),
   is_prime_(D,E,F,C,G,A).
is_prime___split(A,B,C) :-
   is_prime____0(A,B,C).
is_prime_(A,B,C,D,E,F) :-
   A=1,
   B=0,
   C=0,
   is_prime___split(F,E,D).
is_prime(A,B,C,D,E) :-
   A=1,
   B=1,
   C=1.
is_prime(A,B,C,D,E) :-
   A=0,
   B=1,
   C=1.
is_prime(A,B,C,D,E) :-
   A=0,
   B=0,
   C=0.
is_prime__1(A) :-
   true.
is_prime__split(A,B) :-
   C=1,
   D=0,
   E=0,
   F=B+ -1,
   is_prime__1(B),
   is_prime_(C,D,E,B,F,A).
is_prime(A,B,C,D,E) :-
   A=1,
   B=0,
   C=0,
   is_prime__split(E,D).
mult(A,B,C,D,E,F) :-
   A=1,
   B=1,
   C=1.
mult(A,B,C,D,E,F) :-
   A=0,
   B=1,
   C=1.
mult(A,B,C,D,E,F) :-
   A=0,
   B=0,
   C=0.
mult__1(A,B) :-
   true.
mult__3(A,B) :-
   mult__1(A,B),
   A<0.
mult__br2(A,B) :-
   mult__1(A,B),
   A>=0.
mult___0(A,B,C) :-
   D=1,
   E=0,
   F=0,
   G=0-B,
   mult__3(B,C),
   mult(D,E,F,C,G,A).
mult_NodeBlock(A,B) :-
   mult__br2(A,B).
mult_LeafBlock(A,B) :-
   mult_NodeBlock(A,B),
   A<1.
mult_LeafBlock1(A,B) :-
   mult_NodeBlock(A,B),
   A>=1.
mult__br3(A,B) :-
   mult_LeafBlock1(A,B),
   A=1.
mult_NewDefault(A,B) :-
   mult_LeafBlock1(A,B),
   A<1.
mult_NewDefault(A,B) :-
   mult_LeafBlock1(A,B),
   A>1.
mult___0(A,B,C) :-
   mult_LeafBlock(B,C),
   B=0,
   A=0.
mult_NewDefault(A,B) :-
   mult_LeafBlock(A,B),
   A<0.
mult_NewDefault(A,B) :-
   mult_LeafBlock(A,B),
   A>0.
mult___0(A,B,C) :-
   mult__br3(B,C),
   A=1.
mult__8(A,B) :-
   mult_NewDefault(A,B).
mult___0(A,B,C) :-
   mult__8(B,C),
   mult(1,0,0,C,B+ -1,D),
   A=D+C.
mult__split(A,B,C) :-
   mult___0(A,B,C).
mult(A,B,C,D,E,F) :-
   A=1,
   B=0,
   C=0,
   mult__split(F,E,D).
main_entry :-
   true.
main__un(A) :-
   main_entry,
   A+ -1=<46339.
main__un1(A,B,C) :-
   main__un(B),
   is_prime(1,0,0,B,C),
   A+ -1=<46339.
main__un2(A,B,C) :-
   main__un1(A,C,D),
   A+ -1<46340,
   D=1.
main__un3 :-
   main__un2(A,B,C),
   mult(1,0,0,A,B,D),
   D=C,
   A>1,
   B>1.
main_verifier_error :-
   main__un3.

