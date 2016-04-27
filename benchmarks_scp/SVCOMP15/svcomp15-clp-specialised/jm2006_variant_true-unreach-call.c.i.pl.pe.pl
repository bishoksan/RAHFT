main_entry :-
   true.
main__un(A,B,C,D,E) :-
   1*C> -1,
   1*B> -1,
   1*A=0,
   1*B+ -1*D=0,
   1*C+ -1*E=0,
   main_entry.
main__un2(A,B,C,D) :-
   2*A+1*B> -1,
   1*A>=0,
   1*A+ -1*C=0,
   2*A+1*B+ -1*D=0,
   1*E=0,
   main__un(A,B,C,D,E).
main__un1(A,B,C,D,E) :-
   1*A>=0,
   1*C> -1,
   1*A+ -1*C>0,
   2*A+1*B> -1,
   2*A+1*B+ -1*D=0,
   1*A+ -1*C+1*E=0,
   main__un(A,B,C,D,E).
main__un1(A,B,C,D,E) :-
   1*A>=0,
   -1*A+1*C>0,
   2*A+1*B> -1,
   2*A+1*B+ -1*D=0,
   1*A+ -1*C+1*E=0,
   main__un(A,B,C,D,E).
main__un(A,B,C,D,E) :-
   1*A>=1,
   2*A+1*B> -1,
   1*C> -1,
   2*A+1*B+ -1*D=0,
   1*A+ -1*C+1*E=0,
   1*A+ -1*F=1,
   1*B+ -1*G= -2,
   1*A+ -1*C+1*H=1,
   main__un1(F,G,C,D,H).
main__un3(A,B) :-
   1*A>=0,
   1*A+1*B=0,
   1*A+ -1*C=0,
   1*A+ -1*D=0,
   main__un2(A,B,C,D).

