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
main_entry :-
   true.
main__un(A,B,C,D,E) :-
   D=B,
   E=C,
   main_entry,
   E> -1,
   D> -1,
   A=0.
main__un2(A,B,C,D) :-
   main__un(A,B,C,D,E),
   E=0.
main__un1(A,B,C,D,E) :-
   main__un(A,B,C,D,E),
   E<0.
main__un1(A,B,C,D,E) :-
   main__un(A,B,C,D,E),
   E>0.
main__un(A,B,C,D,E) :-
   main__un1(F,G,C,D,H),
   A=F+1,
   B=G+ -2,
   E=H+ -1.
main__un3(A,B) :-
   main__un2(A,B,C,D),
   C=D.
main_precall(A) :-
   main__un3(B,C),
   C=0-B,
   A=1,
   D=0.
main_precall(A) :-
   main__un3(B,C),
   C<0-B,
   A=0,
   D=0.
main_precall(A) :-
   main__un3(B,C),
   C>0-B,
   A=0,
   D=0.
main___VERIFIER_assert(A) :-
   main_precall(A).
main__un4 :-
   A=0,
   main___VERIFIER_assert(A).
main_verifier_error :-
   main__un4.

