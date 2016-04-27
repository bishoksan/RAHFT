main_entry :-
   true.
main__un(A,B) :-
   1*A+ -1*B=0,
   main_entry.
main__un1(A,B) :-
   -1*A> -1024,
   1*A+ -1*B=0,
   main__un(A,B).
main_orig_main_exit(A,B) :-
   1*A>=1024,
   1*A+ -1*B=0,
   main__un(A,B).
main__un(A,B) :-
   -1*A> -1025,
   1*A+ -1*B=0,
   1*A+ -1*C=1,
   1*A+ -1*D=1,
   main__un1(C,D).

