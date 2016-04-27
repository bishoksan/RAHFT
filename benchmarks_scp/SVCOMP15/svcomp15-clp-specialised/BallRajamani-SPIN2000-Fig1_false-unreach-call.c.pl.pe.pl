false :-
   main_verifier_error.
a__1(A) :-
   true.
a__8(A,B) :-
   1*B=0,
   a__1(B).
a__5(A,B) :-
   -1*B>0,
   a__1(B).
a__5(A,B) :-
   1*B>0,
   a__1(B).
a__shadow_mem_0(A,B) :-
   1*C=1,
   1*D=0,
   1*E=0,
   a__5(A,B),
   a(C,D,E,F,G).
a__shadow_mem_0(A,B) :-
   1*B=0,
   a__8(A,B).
a(A,B,C,D,E) :-
   1*A=1,
   1*B=0,
   1*C=0,
   a__shadow_mem_0(E,D).
main_entry :-
   true.
main__un :-
   -1*G>0,
   1*A=1,
   1*B=0,
   1*C=0,
   1*D=1,
   1*E=0,
   1*F=0,
   1*H=0,
   main_entry,
   a(A,B,C,G,I),
   a(D,E,F,G,J).
main__un :-
   1*G>0,
   1*A=1,
   1*B=0,
   1*C=0,
   1*D=1,
   1*E=0,
   1*F=0,
   1*H=0,
   main_entry,
   a(A,B,C,G,I),
   a(D,E,F,G,J).
main_verifier_error :-
   main__un.

