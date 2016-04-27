main_entry :-
   true.
main__un(A,B,C,D) :-
   1*A+ -1*C=0,
   1*B+ -1*D=0,
   main_entry.
main__un2(A,B,C) :-
   1*B>=0,
   1*A+1*B+ -1*C=0,
   1*D=0,
   main__un(A,B,C,D).
main__un1(A,B,C,D) :-
   -1*A+ -1*B+1*C>0,
   -1*A+1*C>=0,
   1*A+1*B+ -1*C+ -1*D=0,
   main__un(A,B,C,D).
main__un1(A,B,C,D) :-
   -1*A+1*C>=0,
   1*A+1*B+ -1*C>0,
   1*A+1*B+ -1*C+ -1*D=0,
   main__un(A,B,C,D).
main__un(A,B,C,D) :-
   -1*A+1*C>=1,
   1*A+1*B+ -1*C+ -1*D=0,
   1*A+ -1*E= -1,
   1*A+1*B+ -1*C+ -1*F= -1,
   main__un1(E,B,C,F).
main__un3(A) :-
   1*B>=0,
   1*A=0,
   1*B+ -1*C=0,
   main__un2(A,B,C).

