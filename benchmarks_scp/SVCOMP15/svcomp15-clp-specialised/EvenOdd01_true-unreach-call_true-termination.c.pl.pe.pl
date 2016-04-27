false :-
   main_verifier_error.
main_entry :-
   true.
main__un :-
   1*A>=0,
   main_entry.
main__un1 :-
   1*A>=0,
   -1*A+1*B>0,
   main__un.
main__un1 :-
   1*A>=0,
   1*A+ -1*B>0,
   main__un.
main_verifier_error :-
   main__un1.

