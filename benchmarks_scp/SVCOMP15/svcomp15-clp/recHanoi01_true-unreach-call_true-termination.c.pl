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
applyHanoi(A,B,C,D) :-
   A=1,
   B=1,
   C=1.
applyHanoi(A,B,C,D) :-
   A=0,
   B=1,
   C=1.
applyHanoi(A,B,C,D) :-
   A=0,
   B=0,
   C=0.
applyHanoi__1(A) :-
   true.
applyHanoi__shadow_mem_0(A) :-
   applyHanoi__1(A),
   A=0.
applyHanoi__call(A) :-
   applyHanoi__1(A),
   A<0.
applyHanoi__call(A) :-
   applyHanoi__1(A),
   A>0.
applyHanoi__shadow_mem_0(A) :-
   B=1,
   C=0,
   D=0,
   E=A+ -1,
   applyHanoi__call(A),
   applyHanoi(B,C,D,E).
applyHanoi(A,B,C,D) :-
   A=1,
   B=0,
   C=0,
   applyHanoi__shadow_mem_0(D).
hanoi(A,B,C,D,E) :-
   A=1,
   B=1,
   C=1.
hanoi(A,B,C,D,E) :-
   A=0,
   B=1,
   C=1.
hanoi(A,B,C,D,E) :-
   A=0,
   B=0,
   C=0.
hanoi__1(A) :-
   true.
hanoi__shadow_mem_0(A,B) :-
   hanoi__1(B),
   B=1,
   A=1.
hanoi__4(A) :-
   hanoi__1(A),
   A<1.
hanoi__4(A) :-
   hanoi__1(A),
   A>1.
hanoi__shadow_mem_0(A,B) :-
   hanoi__4(B),
   hanoi(1,0,0,B+ -1,C),
   A=C*2+1.
hanoi__split(A,B) :-
   hanoi__shadow_mem_0(A,B).
hanoi(A,B,C,D,E) :-
   A=1,
   B=0,
   C=0,
   hanoi__split(E,D).
main_entry :-
   true.
main__un(A) :-
   main_entry,
   A+ -1=<30.
main__un1 :-
   main__un(A),
   applyHanoi(1,0,0,A),
   hanoi(1,0,0,A,B),
   B<C.
main__un1 :-
   main__un(A),
   applyHanoi(1,0,0,A),
   hanoi(1,0,0,A,B),
   B>C.
main_verifier_error :-
   main__un1.

