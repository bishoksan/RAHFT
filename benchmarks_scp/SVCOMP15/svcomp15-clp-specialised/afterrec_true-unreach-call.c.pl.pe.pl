main_entry :-
   true.
main_precall :-
   1*A=0,
   main_entry.
main_f(A) :-
   1*A=2,
   main_precall.

