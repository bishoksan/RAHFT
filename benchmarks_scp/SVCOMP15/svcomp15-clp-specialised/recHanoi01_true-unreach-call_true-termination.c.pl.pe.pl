false :-
   main_verifier_error.
applyHanoi__1(A) :-
   -1*A>= -31.
applyHanoi__shadow_mem_0(A) :-
   1*A=0,
   applyHanoi__1(A).
applyHanoi__call(A) :-
   -1*A>0,
   applyHanoi__1(A).
applyHanoi__call(A) :-
   -1*A>= -31,
   1*A>0,
   applyHanoi__1(A).
applyHanoi__shadow_mem_0(A) :-
   -1*A>= -31,
   1*A>=1,
   1*B=1,
   1*C=0,
   1*D=0,
   1*A+ -1*E=1,
   applyHanoi__call(A),
   applyHanoi(B,C,D,E).
applyHanoi(A,B,C,D) :-
   -1*D>= -31,
   1*D>=0,
   1*A=1,
   1*B=0,
   1*C=0,
   applyHanoi__shadow_mem_0(D).
hanoi__1(A) :-
   -1*A>= -31.
hanoi__shadow_mem_0(A,B) :-
   1*A=1,
   1*B=1,
   hanoi__1(B).
hanoi__4(A) :-
   -1*A> -1,
   hanoi__1(A).
hanoi__4(A) :-
   -1*A>= -31,
   1*A>1,
   hanoi__1(A).
hanoi__shadow_mem_0(A,B) :-
   1*A+ -4*B>= -5,
   1*A+ -8*B>= -17,
   -1*B>= -31,
   1*B>=2,
   1*C=1,
   1*D=0,
   1*E=0,
   1*B+ -1*F=1,
   1*A+ -2*G=1,
   hanoi__4(B),
   hanoi(C,D,E,F,G).
hanoi__split(A,B) :-
   1*A+ -4*B>= -5,
   1*A+ -8*B>= -17,
   1*B>=1,
   -1*B>= -31,
   1*A+ -2*B>= -1,
   hanoi__shadow_mem_0(A,B).
hanoi(A,B,C,D,E) :-
   -1*D>= -31,
   -2*D+1*E>= -1,
   -4*D+1*E>= -5,
   -8*D+1*E>= -17,
   1*D>=1,
   1*A=1,
   1*B=0,
   1*C=0,
   hanoi__split(E,D).
main_entry :-
   true.
main__un(A) :-
   -1*A>= -31,
   main_entry.
main__un1 :-
   1*G+ -4*I>= -5,
   1*I>=1,
   -1*I>= -31,
   -1*G+1*H>0,
   1*G+ -2*I>= -1,
   1*A=1,
   1*B=0,
   1*C=0,
   1*D=1,
   1*E=0,
   1*F=0,
   main__un(I),
   applyHanoi(A,B,C,I),
   hanoi(D,E,F,I,G).
main__un1 :-
   1*G+ -4*I>= -5,
   1*G+ -1*H>0,
   1*I>=1,
   -1*I>= -31,
   1*G+ -2*I>= -1,
   1*A=1,
   1*B=0,
   1*C=0,
   1*D=1,
   1*E=0,
   1*F=0,
   main__un(I),
   applyHanoi(A,B,C,I),
   hanoi(D,E,F,I,G).
main_verifier_error :-
   main__un1.

