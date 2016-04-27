false :-
   main_verifier_error.
main_entry :-
   true.
main__un(A,B,C,D,E) :-
   1*A=0,
   1*B=0,
   1*C=0,
   1*D=1,
   1*E=1,
   main_entry.
main__un(A,B,C,D,E) :-
   -1*C>0,
   1*A=0,
   1*B=0,
   1*D=1,
   1*E=0,
   main_entry.
main__un(A,B,C,D,E) :-
   1*C>0,
   1*A=0,
   1*B=0,
   1*D=1,
   1*E=0,
   main_entry.
main__un2(A,B,C) :-
   1*E>=0,
   1*A>=0,
   1*D=0,
   main__un(A,B,C,F,E).
main__un1(A,B,C,D,E) :-
   1*E>=0,
   -1*F>0,
   1*A>=0,
   main__un(A,B,C,D,E).
main__un1(A,B,C,D,E) :-
   1*E>=0,
   1*F>0,
   1*A>=0,
   main__un(A,B,C,D,E).
main__un(A,B,C,D,E) :-
   1*E>=2,
   1*A>=1,
   1*A+ -1*F=1,
   1*B+1*E+ -1*G+ -1*H=2,
   1*E+ -1*I=2,
   1*B+ -1*D+1*E+ -1*G+ -1*J=2,
   main__un1(F,G,C,H,I).
main__un3(A,B) :-
   -1*C>0,
   1*A>=0,
   main__un2(A,B,C).
main__un3(A,B) :-
   1*C>0,
   1*A>=0,
   main__un2(A,B,C).
main_precall(A) :-
   -1*B+1*C>0,
   1*B>=0,
   1*A=0,
   1*D=0,
   main__un3(B,C).
main_precall(A) :-
   1*B+ -1*C>0,
   1*B>=0,
   1*A=0,
   1*D=0,
   main__un3(B,C).
main___VERIFIER_assert(A) :-
   1*A=0,
   main_precall(A).
main__un4 :-
   1*A=0,
   main___VERIFIER_assert(A).
main_verifier_error :-
   main__un4.

