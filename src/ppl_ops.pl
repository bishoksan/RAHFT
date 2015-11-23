:- module(ppl_ops,_).

:- use_module(library(ppl)).

% Constraint relations using PPL

getConstraint(H,Cs0) :-
	ppl_Polyhedron_get_minimized_constraints(H,Cs0).
	
makePolyhedron(Cs,H1)  :-
	ppl_new_NNC_Polyhedron_from_constraints(Cs,H1).
	
copyPolyhedron(H1,H2)  :-
	ppl_new_NNC_Polyhedron_from_NNC_Polyhedron(H1,H2).
	
satisfiable(Cs,H1) :-
	ppl_new_NNC_Polyhedron_from_constraints(Cs,H1),
	\+ ppl_Polyhedron_is_empty(H1).
			
project(H,Zs,H) :-
	ppl_Polyhedron_remove_space_dimensions(H,Zs).
	
equivalent(H0,H1) :-
	ppl_Polyhedron_equals_Polyhedron(H0,H1).
	
disjointFrom(H0,H1) :-
	ppl_Polyhedron_is_disjoint_from_Polyhedron(H0,H1).

entails(H0,_) :-
	ppl_Polyhedron_is_universe(H0),
	!.
entails(H0,H1) :-
	ppl_Polyhedron_contains_Polyhedron(H0,H1).
	
consistent(H0,H1) :-
	copyPolyhedron(H0,H2),
	ppl_Polyhedron_intersection_assign(H2,H1),
	\+ ppl_Polyhedron_is_empty(H2).
	
convhull(empty,H1,H1) :-
	!.
convhull(H1,empty,H1) :-
	!.
convhull(H0,H1,H2) :-
	ppl_Polyhedron_poly_hull_assign(H1,H0),
	H2 = H1.
	
widenPolyhedraH79(H0,H1) :-
	ppl_Polyhedron_H79_widening_assign(H0,H1).
	
widenPolyhedraBHRZ03(H0,H1) :-
	ppl_Polyhedron_BHRZ03_widening_assign(H0,H1).
	
widenUptoH79(H0,H1,Cs) :-
	ppl_Polyhedron_bounded_H79_extrapolation_assign(H0,H1,Cs).
	
widenUptoBHRZ03(H0,H1,Cs) :-
	ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign(H0,H1,Cs).
	
isEmpty(H) :-
	ppl_Polyhedron_is_empty(H).

mapCoords(H,Map) :-
	ppl_Polyhedron_map_space_dimensions(H,Map).


rankingFunction_PR(H,F) :-
	ppl_one_affine_ranking_function_PR_NNC_Polyhedron(H,F).
	
rankingFunction_MS(H,F) :-
	ppl_one_affine_ranking_function_MS_NNC_Polyhedron(H,F).

	
polyhedronDimension(H,K) :-
	ppl_Polyhedron_space_dimension(H,K).

maximizeExpr(P, Expr, CoNum, CoDen):-
    ppl_Polyhedron_maximize(P, Expr, CoNum, CoDen, _).

dropNonIntegerPoint(H0):-
    ppl_Polyhedron_drop_some_non_integer_points(H0,polynomial).

start_ppl :-
	ppl_initialize,
	ppl_version(Pv),
	write('PPL version used: '),
	write(Pv),
	nl.
	
end_ppl :-
	ppl_finalize.
