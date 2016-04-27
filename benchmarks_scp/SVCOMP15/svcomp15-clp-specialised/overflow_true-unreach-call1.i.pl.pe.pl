main_entry :-
   true.
main__un(A) :-
   1*A=10,
   main_entry.
main__un1(A) :-
   1*A>=10,
   main__un(A).
main__un(A) :-
   1*A>=12,
   1*A+ -1*B=2,
   main__un1(B).

