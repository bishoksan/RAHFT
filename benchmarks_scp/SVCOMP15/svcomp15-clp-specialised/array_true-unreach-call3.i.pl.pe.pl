main_entry :-
   true.
main__un(A) :-
   1*A=0,
   main_entry.
main_orig_main_exit(A) :-
   -1*A>= -1024,
   1*A>=0,
   1*B=0,
   main__un(A).
main_orig_main_exit(A) :-
   -1*A>= -1024,
   1*A>1023,
   main__un(A).
main__un1(A) :-
   -1*B>0,
   -1*A>= -1023,
   1*A>=0,
   main__un(A).
main__un1(A) :-
   1*B>0,
   -1*A>= -1023,
   1*A>=0,
   main__un(A).
main__un(A) :-
   -1*A>= -1024,
   1*A>=1,
   1*A+ -1*B=1,
   main__un1(B).

