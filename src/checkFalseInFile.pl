:- module(checkFalseInFile,[checkForFalse/2], []).

:- use_module(chclibs(program_loader)).

% Check the presence of false in the clauses, if not present returns
% "safe" otherwise "unknown".
checkForFalse(F, Result):-
	load_file(F),
	( my_clause(false, _, _)->
	    Result=unknown
        ; Result=safe
	).

