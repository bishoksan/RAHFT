:- module(checkSafety,_).


safe(PFile) :-
	open(PFile,read,S),
	read(S,C),
	(checkForFalse(S,C) -> 
		close(S); 
		close(S),
		fail).

	
checkForFalse(_,end_of_file) :-
	!.
checkForFalse(S,(H:-_)) :-
	H \== false,
	H \== false_ans,
	read(S,C1),
	checkForFalse(S,C1).
	
main([F]) :-
	open('pec_benchmarks.txt',append,S),
	write(S,F), 
	(safe(F) -> 
		write(S,': PROGRAM IS SAFE'),nl(S), halt(0); 
		write(S,': PROGRAM MIGHT NOT BE SAFE'),nl(S), halt(1)),
	close(S).
	

	
