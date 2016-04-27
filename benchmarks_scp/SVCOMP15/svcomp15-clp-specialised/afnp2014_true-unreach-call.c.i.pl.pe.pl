main_entry :-
   true.
main__un(A,B) :-
   1*A=1,
   1*B=0,
   main_entry.
main__un1(A,B) :-
   1*A+ -2*B>= -2,
   1*B>=0,
   1*A+ -1*B>=0,
   -1*B> -1000,
   1*A>=1,
   main__un(A,B).
main_orig_main_exit(A,B) :-
   1*B>=1000,
   -1*B> -1001,
   1*A+ -2*B>= -2,
   main__un(A,B).
main_orig_main_exit(A,B) :-
   1*B>=0,
   1*A+ -1*B>=0,
   -1*B> -1000,
   1*A>=1,
   1*C=0,
   main__un1(A,B).
main__un2(A,B) :-
   1*A+ -1*B>=0,
   1*B>=0,
   -1*C>0,
   -1*B> -1000,
   1*A>=1,
   main__un1(A,B).
main__un2(A,B) :-
   1*A+ -1*B>=0,
   1*B>=0,
   1*C>0,
   -1*B> -1000,
   1*A>=1,
   main__un1(A,B).
main__un(A,B) :-
   1*A+ -2*B>= -2,
   1*B>=1,
   -1*B> -1001,
   1*A+ -1*B>=0,
   1*A+ -1*B+ -1*C= -1,
   1*B+ -1*D=1,
   main__un2(C,D).

