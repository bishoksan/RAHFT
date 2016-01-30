% get_options/3 provided by Michael Leuschel
get_options([],[],[]).
get_options([X|T],Options,Args) :-
	( recognised_option(X,Opt,Values) ->
	    append(Values, Rest, T),
	    RT = Rest,
	    Options = [Opt|OT], Args = AT
	; Options = OT, Args = [X|AT],
	  RT = T
	),
	get_options(RT,OT,AT).

