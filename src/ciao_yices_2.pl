% Ciao Prolog foreign language interface for Yices 2.3.1
% See http://yices.csl.sri.com/

:- module(ciao_yices_2,[
			yices_version/1,
			yices_init/0,
			yices_exit/0,
			yices_reset/0,
			yices_context/1,
			yices_free_context/1,
			yices_parse_term/2,
			yices_assert_formula/3,
			yices_check/2,
			yices_context_status/2,
			yices_status/2,
			yices_declare_real/1,
			yices_declare_int/1,
			yices_declare_bool/1,
			yices_term_is_bool/2,
			yices_error_string/1,
			yices_get_model/3,
			yices_get_int32_value/4,
			yices_get_term_by_name/2,
			yices_get_value_as_term/3,
			yices_formula_true_in_model/3,
            yices_term_to_string/5
            ]).

:- use_package([
        assertions,
        basicmodes,
        regtypes,
        foreign_interface
]).

:- true pred yices_version(go(V)) :: 
 		string + (foreign, returns(V)).
 		
:- true pred yices_init + foreign.

:- true pred yices_exit + foreign.

:- true pred yices_reset + foreign.
 		
:- true pred yices_new_context(in(_),go(Ctx)) :: 
 		address * address + (foreign, returns(Ctx)).
 		
:- true pred yices_free_context(in(_)) :: 
 		address + foreign.
 		
:- true pred yices_set_term_name(in(_),in(_),go(TermIndex)) :: 
 		int * string * int + (foreign, returns(TermIndex)).
 		              
:- true pred yices_parse_term(in(_),go(TermIndex)) :: 
 		string * int + (foreign, returns(TermIndex)).
 		
:- true pred yices_assert_formula(in(_),in(_), go(Status)) :: 
 		address * int * int + (foreign, returns(Status)).
 		
:- true pred yices_check_context(in(_),in(_),go(Sat)) :: 
 		address * address * int + (foreign, returns(Sat)).
 		
:- true pred yices_context_status(in(_),go(Status)) :: 
 		address * int + (foreign, returns(Status)).
 		
:- true pred yices_new_uninterpreted_term(in(_),go(V)) ::
		address * int + (foreign, returns(V)).	
		
:- true pred yices_new_variable(in(_),go(V)) ::
		address * int + (foreign, returns(V)).		  
		
:- true pred yices_real_type(go(Real)) ::
		address + (foreign, returns(Real)).	
	
:- true pred yices_int_type(go(Int)) ::
		address + (foreign, returns(Int)).	
		
:- true pred yices_bool_type(go(Bool)) ::
		address + (foreign, returns(Bool)).	
		
:- true pred yices_term_is_bool(in(_),go(B)) ::
		int * int + (foreign, returns(B)).	
		
:- true pred yices_error_string(go(E)) ::
		string + (foreign, returns(E)).		
			
:- true pred yices_get_model(in(_),in(_),go(Model)) ::
		address * int * address + (foreign, returns(Model)).
		
:- true pred yices_get_int32_value(in(_),in(_),in(_),go(Status)) ::
		address * int * int * int + (foreign, returns(Status)).	
		
:- true pred yices_get_term_by_name(in(_),go(Term)) ::
		string * int + (foreign, returns(Term)).
		
:- true pred yices_get_value_as_term(in(_),in(_),go(Term)) ::
		address * int * int + (foreign, returns(Term)).	
		
:- true pred yices_formula_true_in_model(in(_),in(_),go(TF)) ::
		address * int * int + (foreign, returns(TF)).	

:- true pred yices_term_to_string(in(_),in(_), in(_),in(_), go(TF)) ::
		 int * int* int * int* string + (foreign, returns(TF)).


:- use_foreign_library(yices). 	% library is in /usr/local/lib

yices_context(Ctx) :-
	null(Null),
	yices_new_context(Null,Ctx).
	
yices_check(Ctx,StatusName) :-
	null(Null),
	yices_check_context(Ctx,Null,Status),
	status(Status,StatusName).
	
yices_declare_real(X) :-
	yices_real_type(Real),
	yices_new_uninterpreted_term(Real,V),
	yices_set_term_name(V,X,Status),
	(Status == 0 -> true; write('Failed to declare '),write(X),nl,fail). 
	
yices_declare_int(X) :-
	yices_int_type(Int),
	yices_new_uninterpreted_term(Int,V),
	yices_set_term_name(V,X,Status),
	(Status == 0 -> true; write('Failed to declare '),write(X),nl,fail). 
	
yices_declare_bool(X) :-
	yices_bool_type(Bool),
	yices_new_uninterpreted_term(Bool,V),
	yices_set_term_name(V,X,Status),
	(Status == 0 -> true; write('Failed to declare '),write(X),nl,fail). 
	
yices_status(Ctx,StatusName) :-
	yices_context_status(Ctx,Status),
	status(Status,StatusName).
	
status(0,idle).
status(1,searching).
status(2,unknown).
status(3,satisfiable).
status(4,unsatisfiable).
status(5,interrupted).
status(6,error).