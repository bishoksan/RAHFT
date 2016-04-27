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
a(A,B,C,D,E) :-
   A=1,
   B=1,
   C=1.
a(A,B,C,D,E) :-
   A=0,
   B=1,
   C=1.
a(A,B,C,D,E) :-
   A=0,
   B=0,
   C=0.
a__1(A) :-
   true.
a__8(A,B) :-
   a__1(B),
   B=0.
a__5(A,B) :-
   a__1(B),
   B<0.
a__5(A,B) :-
   a__1(B),
   B>0.
a__shadow_mem_0(A,B) :-
   C=1,
   D=0,
   E=0,
   a__5(A,B),
   a(C,D,E,F,G).
a__shadow_mem_0(A,B) :-
   a__8(A,B).
a(A,B,C,D,E) :-
   A=1,
   B=0,
   C=0,
   a__shadow_mem_0(E,D).
main_entry :-
   true.
main__un :-
   A=1,
   B=0,
   C=0,
   main_entry,
   D<0,
   D=0,
   E=1,
   a(1,0,0,D,F),
   a(A,B,C,D,G).
main__un :-
   A=1,
   B=0,
   C=0,
   main_entry,
   D<0,
   D<0,
   E=0,
   a(1,0,0,D,F),
   a(A,B,C,D,G).
main__un :-
   A=1,
   B=0,
   C=0,
   main_entry,
   D<0,
   D>0,
   E=0,
   a(1,0,0,D,F),
   a(A,B,C,D,G).
main__un :-
   A=1,
   B=0,
   C=0,
   main_entry,
   D>0,
   D=0,
   E=1,
   a(1,0,0,D,F),
   a(A,B,C,D,G).
main__un :-
   A=1,
   B=0,
   C=0,
   main_entry,
   D>0,
   D<0,
   E=0,
   a(1,0,0,D,F),
   a(A,B,C,D,G).
main__un :-
   A=1,
   B=0,
   C=0,
   main_entry,
   D>0,
   D>0,
   E=0,
   a(1,0,0,D,F),
   a(A,B,C,D,G).
main_verifier_error :-
   main__un.

