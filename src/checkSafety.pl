:- module(checkSafety, [checkSafety/2]).

:- module(checkSafety,_).

safe(PFile) :-
	open(PFile,read,S),
	read(S,C),
	( checkForFalse(S,C) -> 
	    close(S)
	; close(S),
	  fail
	).

checkForFalse(_,end_of_file) :-
	!.
checkForFalse(S,(H:-_)) :-
	H \== false,
	H \== false_ans,
	read(S,C1),
	checkForFalse(S,C1).

/*
main([F]) :-
	open('/tmp/fta_ref_intaut.pl',append,S),
	%write(S,F),
	(safe(F) -> 
		write(S,':safe'),nl(S), halt(0);
		write(': PROGRAM MIGHT NOT BE SAFE'),nl, halt(1)),
	close(S).
*/

checkSafety(F Result) :-
	S = user_output,
	write(S,F), 
	( safe(F, K) ->
	    write(S,': PROGRAM IS SAFE'),nl(S), Result = safe
	;
	    write(S,': PROGRAM MIGHT NOT BE SAFE'),nl(S), Result = otherwise % unsafe or unknown
	).

	
