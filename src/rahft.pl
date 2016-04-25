%check -model option, it is printing result of each iterations
:- module(rahft, [main/1], []).

% Solves a non-linear Horn clause using a linear solver.
% Input: a set of Horn clauses
% Output: safe if the program is solved else unknown if it is not solved

:- use_module(library(format), [format/2, format/3]).
:- use_module(library(system_extra), [mkpath/1,mktempdir_in_tmp/2, rmtempdir/1]).
:- use_module(library(system)). % mktemp_in_tmp is available here
:- use_module(library(pathnames), [path_basename/2, path_concat/3, path_split/3]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(lists)).

:- use_module(chclibs(thresholds1), [main/1]).
:- use_module(chclibs(load_simple)).
:- use_module(chclibs(cpascc), [main/1]).
:- use_module(chclibs(qa), [main/1]).
:- use_module(chclibs(common)).

:- use_module(counterExample, [main/1]).
:- use_module(insertProps, [main/1]).
:- use_module(genfta, [main/1]).
:- use_module(splitClauseIds, [main/1]).
:- use_module(ftaRefine, [main/1]).
:- use_module(interpolantAutomaton, [main/1]).
:- use_module(checkFalseInFile, [checkForFalse/2]).
:- use_module(integerProgram, [main/1]).

:- use_module(library(write)).
:- use_module(library(read)).

:- use_module(library(source_tree), [remove_dir/1]).

:- use_module(library(process)). % invoking external processes

:- include(chclibs(get_options)).

% stores output of the tool
logfile('result.txt').

% (debug)
% go :-
% 	rahft:main(['/Users/kafle/Desktop/RAHFT/examples/running.nts.pl']).

% ---------------------------------------------------------------------------
% Main
% ---------------------------------------------------------------------------

:- data opt_array/0.
:- data opt_debug_temps/0.

% printing output of RAHFT
displayHelpMenu:-
	help_msg(Str),
	format(user_error, "~s~n", [Str]).

help_msg(
"Usage: rahft <prog> [<Options>]

Options:
 -help    display this help menu
 -int     uses interpolant automaton for trace generalisation during refinement
 -model   show model
 -array   enable array constraints
 -sp      only horn specialization
 -itr N   limit abstract refine iterations 

 -debug-temps    keep files for intermediate passes (debug)
").

recognised_option('-help',  help, []).
recognised_option('-model', model, []).
recognised_option('-int',   int, []).
recognised_option('-array', array, []).
recognised_option('-sp',    horn_specialise(F), [F]).
recognised_option('-itr',   bounded(N), [N]).
recognised_option('-debug-temps', debug_temps, []).

main(ArgV) :-
	get_options(ArgV,Options,Args0),
	( member(help, Options) ->
	    displayHelpMenu
	; \+ Args0 = [_] -> % wrong args
	    displayHelpMenu
	; Args0 = [F],
	  cleanup,
	  main_(Options, F)
	).

cleanup :-
	retractall_fact(opt_array).

main_(Options, Prog) :-
	member(horn_specialise(OFile), Options),
	!,
	hornSpecialise(Prog, OFile).
main_(Options, Prog) :-
	( member(model, Options) ->
	    ShowModel = yes
	; ShowModel = no
	),
	( member(int, Options) ->
	    WithInterpolant = yes
	; WithInterpolant = no
	),
	( member(array, Options) ->
	    assertz_fact(opt_array)
	; true
	),
	( member(bounded(N), Options) ->
	    convert2num(N,N1),
	    Bounded = bounded(N1)
	; Bounded = unbounded
	),
	( member(debug_temps, Options) ->
	    assertz_fact(opt_debug_temps)
	; true
	),
	applyRAHFT(Prog, WithInterpolant, ShowModel, Bounded).

% ---------------------------------------------------------------------------
% Horn clause pre-processing
% ---------------------------------------------------------------------------

preProcessHorn(Prog, F_Int, F_QA, QACPA, F_WidenPoints, F_Threshold, OutputFile):-
	( opt_array ->
	    % TODO:{arrays(1): "preprocess"}
	    %   for each clause in P, apply 
	    %    - constraint replacement algorithm of [1] (Section 5.2, page 338 RR1-WR3);
	    %     step 1 of [1] (Section 5.3, page 344) // Note: This is “delete all write constraints”
	    %   producing P2;
	    format("Preprocessing array constraints~n", []),
	    Prog2 = Prog,
	    % TODO:{arrays(2): "integer_program"}
	    format("Getting integer program~n", []),
	    Prog2Int = F_Int,
	    integerProgram:main(['-prg', Prog2, '-o', Prog2Int])
	; Prog2 = Prog,
	  Prog2Int = Prog2
	),
	format("Computing query-answer transformation ~n", []),
	qa:main([Prog2Int, '-query', 'false', '-ans',  '-o', F_QA]),
	format("Computing widening thresholds for QA program~n", []),
	thresholds1:main(['-prg', F_QA, '-o', F_Threshold]),
	format("Analyse QA program~n", []),
	cpascc:main(['-prg', F_QA, '-withwut', 'bounded', '-wfunc', 'h79', '-widenpoints',F_WidenPoints,'-threshold', F_Threshold, '-o', QACPA]),
	% NOTE: Using Prog2, not Prog2Int!
	( opt_array -> % TODO:{arrays} add more control over strengthening
	    insertProps:main(['-array', '-prg', Prog2, '-props', QACPA, '-o', OutputFile])
	; insertProps:main(['-prg', Prog2, '-props', QACPA, '-o', OutputFile])
	).

% ---------------------------------------------------------------------------
% Analysis using CPA
% ---------------------------------------------------------------------------

verifyCPA(Prog, F_Int, F_QA, QACPA, F_CPA, OutputFile, F_WidenPoints, F_Traceterm, F_Threshold,Result) :-
	preProcessHorn(Prog, F_Int, F_QA, QACPA, F_WidenPoints, F_Threshold, OutputFile),
	format("Checking for the presence of false clauses~n", []),
	checkFalseInFile:checkForFalse(OutputFile, Result1),
	( Result1=safe -> % no (false :- ...)
	    % TODO:{arrays} "if there is no trace for false in A_P' then return safe" (is it equivalent?)
	    Result=safe
	; 
	    ( opt_array ->
	        % TODO:{arrays(4) - "abstract_analyse"}
	        % for each clause in P, apply
	        %     constraint generalisation algorithm (step 2-3) of [1] (Section 5.3, page 344)
	        %   producing P2;
	        format("Generalizing constraint arrays~n", []),
		OutputFile2 = OutputFile,
	        % TODO:{arrays(2): "integer_program"}
	        %   compute integer program ProgInt (use it in qa,thresholds1,cpascc; but not in insertProps)
	        format("Getting integer program~n", []),
	        atom_concat(OutputFile2, '.int.pl', OutputFile2Int),
	        integerProgram:main(['-prg', OutputFile2, '-o', OutputFile2Int])
	    ; OutputFile2 = OutputFile,
	      OutputFile2Int = OutputFile2
	    ),
	    %
	    format("Computing widening thresholds for PE program~n", []),
	    thresholds1:main(['-prg', OutputFile2Int, '-o', F_Threshold]),
	    format("Analyse PE program~n", []),
	    cpascc:main(['-prg', OutputFile2Int, '-withwut', 'bounded', '-wfunc', 'h79', '-widenpoints',F_WidenPoints, '-threshold', F_Threshold, '-cex', F_Traceterm, '-o', F_CPA]),
	    format("Analysing counterexample~n", []),
	    ( opt_array ->
	        % TODO:{arrays(5) - in counterExample.pl} "need extension to the theory of arrays"
	        write(user_output, 'TODO: use arrays in cex'), nl(user_error),
	        counterExample:main([OutputFile, F_Traceterm, Result])
	    ; counterExample:main([OutputFile, F_Traceterm, Result])
	    )
	).

% ---------------------------------------------------------------------------

:- use_module(library(system), [file_exists/1]).
:- use_module(library(bundle/paths_extra), [fsR/2]).

% Find determinise.jar in the same directory as the executable
% or in the sources.
determinise_jar(Path) :-
	( current_executable(ExecPath),
	  path_split(ExecPath, Dir, _)
	; fsR(bundle_src('RAHFT')/src, Dir)
	),
	path_concat(Dir, 'determinise.jar', Jar),
	file_exists(Jar),
	!,
	Path = Jar.

% ---------------------------------------------------------------------------
% Refinement
% ---------------------------------------------------------------------------

refineHorn(F_SP, F_FTA, F_DFTA,  F_SPLIT, F_TRACETERM, F_REFINE, WithInterpolant):-
        format( "Generate FTA from program and error trace~n", []),
        genfta:main(['-prg', F_SP, '-trace', F_TRACETERM, '-o', F_FTA]),
        ( WithInterpolant=yes ->
            format( "Computing interpolant automaton from an error trace~n", []),
            interpolantAutomaton:main(['-prg', F_SP,  '-trace',  F_TRACETERM, '-o',  F_FTA])
	; true
        ),
	determinise_jar(DeterminiseJar),
	process_call(path(java), ['-jar', DeterminiseJar, F_FTA, '-nodc', '-show', '-o', F_DFTA], []),
        format( "Find disjoint clauses ~n", []),
        splitClauseIds:main(['-prg', F_SP, '-o', F_SPLIT]),
        format( "Refining using DFTA ~n", []),
        ftaRefine:main(['-prg', F_SP, '-fta', F_DFTA, '-split', F_SPLIT, F_SPLIT, '-o', F_REFINE]).

% ---------------------------------------------------------------------------
% printing output of RAHFT
% ---------------------------------------------------------------------------

printRahftOutput(LogS, Prog, Safety, Iteration, Time):-
	printRahftOutput_(LogS, Prog, Safety, Iteration, Time),
	printRahftOutput_(user_output, Prog, Safety, Iteration, Time).

printRahftOutput_(LogS, Prog, Safety, Iteration, Time):-
	format(LogS, 'RAHFT: {', []),
	format(LogS, 'Program: ~w, ', [Prog]),
	format(LogS, 'Safety: ~w, ', [Safety]),
	format(LogS, 'Iteration: ~w, ', [Iteration]),
	format(LogS, 'Time: ~w millisecs.} ~n', [Time]).

% ---------------------------------------------------------------------------
% main procedure RAHFT
% ---------------------------------------------------------------------------

applyRAHFT(Prog1, WithInterpolant, ShowModel, Bounded) :-
	logfile(LogFile),
	open(LogFile, append, LogS),
	%creating temporary directory for intermediate files
	prepare_resultdir(Prog1, ResultDir),
	K = 0,
	path_basename(Prog1, F),
	createTmpFilePP(ResultDir, F, F_Int, F_QA, QA_CPA, F_CPA, F_SP,F_WidenPoints, F_TRACETERM, F_THRESHOLD),
	createTmpFileRef(ResultDir, F, F_FTA, F_DFTA, F_SPLIT, F_REFINE),
	%
	statistics(runtime,[START|_]),
	abstract_refine(Bounded, LogS,  Prog1, K, Result, K1, WithInterpolant, F_Int, F_QA, QA_CPA, F_CPA, F_SP,F_WidenPoints, F_TRACETERM, F_THRESHOLD, F_FTA, F_DFTA, F_SPLIT, F_REFINE),
	statistics(runtime,[END|_]),
	( ShowModel = no -> true
	; ( Result=safe ->
              showModel(QA_CPA, F_CPA, Prog1, F_REFINE)
	  ; ( Result=unsafe ->
	        write('There is no model since the program is unsafe'), nl
            ; write('We do not know if there exists a model for the program'), nl
            )
	  )
	),
	DIFF is END - START,
	path_basename(Prog1, F),
	printRahftOutput(LogS,F, Result, K1, DIFF),
	%
	end_resultdir(ResultDir),
	close(LogS).

abstract_refine(bounded(Itr), _, _, K, unknown, Itr, _, _, _, _, _, _,_, _, _, _, _, _, _):-
	K > Itr, % Exceeded allowed number of iterations, stop
	!.
abstract_refine(Bounded, LogS,  Prog1, K, Result, K2, WithInterpolant, F_Int, F_QA, QA_CPA, F_CPA, F_SP,F_WidenPoints, F_TRACETERM, F_THRESHOLD, F_FTA, F_DFTA, F_SPLIT, F_REFINE) :-
	verifyCPA(Prog1, F_Int, F_QA, QA_CPA, F_CPA, F_SP, F_WidenPoints, F_TRACETERM, F_THRESHOLD, Ret1),
	( Ret1 = safe ->
	    Result = Ret1,
	    K2 = K,
	    format("the program is safe~n", [])
	; Ret1 = unsafe ->
	    Result= Ret1,
	    K2 = K,
	    format("the program is unsafe~n", [])
	; % refinement with FTA
	  refineHorn(F_SP, F_FTA, F_DFTA,  F_SPLIT, F_TRACETERM, F_REFINE, WithInterpolant),
	  K1 is K + 1,
	  abstract_refine(Bounded, LogS,  F_REFINE, K1, Result, K2, WithInterpolant, F_Int, F_QA, QA_CPA, F_CPA, F_SP,F_WidenPoints, F_TRACETERM, F_THRESHOLD, F_FTA, F_DFTA, F_SPLIT, F_REFINE)
	).

hornSpecialise(Prog, OutputFile):-
	prepare_resultdir(Prog, ResultDir),
	path_basename(Prog, F),
	createTmpFilePP(ResultDir, F, F_Int, F_QA, QA_CPA,_,_,F_WidenPoints, _, F_THRESHOLD),
	statistics(runtime,[START|_]),
	preProcessHorn(Prog, F_Int, F_QA, QA_CPA, F_WidenPoints, F_THRESHOLD, OutputFile),
	statistics(runtime,[END|_]),
	DIFF is END - START,
	path_basename(Prog, F),
	format( "Total time: ~w ms. ~n", [DIFF]),
	end_resultdir(ResultDir).

% ---------------------------------------------------------------------------
% (Temporary directory for intermediate passes)

prepare_resultdir(Prog, ResultDir) :-
	( opt_debug_temps ->
	    atom_concat(Prog, '_output', ResultDir),
	    mkpath(ResultDir)
	; mktempdir_in_tmp('rahft-XXXXXXXX', ResultDir)
	).

end_resultdir(ResultDir) :-
	( opt_debug_temps ->
	    format("NOTE: Files for temporary results are kept at ~w~n", [ResultDir])
	; % remove the directory of intermediate files
	  ( file_exists(ResultDir) -> rmtempdir(ResultDir)
	  ; true
	  )
	).

% ---------------------------------------------------------------------------

% if F_CPA exists it shows the model from F_CPA, else from QA_CPA
% if F_REFINE exists then the model corresponds to this else to Prog

showModel(QA_CPA, F_CPA,Prog, F_REFINE):-
	write('Model: '), nl,
	( file_exists(F_CPA) -> showInv(F_CPA) ; showInvQA(QA_CPA) ),
	nl,
	write('For the program: '), nl,
	( file_exists(F_REFINE) -> showProg(F_REFINE) ; showProg(Prog) ),
	nl.

showInv(F):-
	open(F, read, S),
	read(S, Inv),
	writeToConsole(S, Inv),
	close(S).

writeToConsole(_, end_of_file):-
	!.
writeToConsole(S, Inv):-
	numbervars(Inv, 0, _),
	write('.'),
	nl,
	read(S, Inv1),
	writeToConsole(S, Inv1).

showProg(F):-
	load_file(F),
	writeCls.

writeCls:-
	my_clause(H, B, _),
	numbervars((H, B), 0, _),
	writeq(H),
	write(' :- '),
	list2Conj(B, B1),
	write(B1),
	write('.'),
	nl,
	fail.
writeCls.


showInvQA(F):-
	open(F, read, S),
	read(S, Inv),
	writeToConsoleQA(S, Inv),
	close(S).

writeToConsoleQA(_, end_of_file):-
	!.
writeToConsoleQA(S, (H:-Inv)):-
	stripSuffix(H, H1),
	numbervars((H1:-Inv), 0, _),
	write((H1:-Inv)),
	write('.'),
	nl,
	read(S, Inv1),
	writeToConsoleQA(S, Inv1).

stripSuffix(F,F1) :-
	F =.. [P|Xs],
	name(P,PName),
	removeSuffixChars(PName,P1Name),
	name(P1,P1Name),
	F1 =.. [P1|Xs],
	!.
	
removeSuffixChars(FName,F1Name) :-
	append("_query",_,Suff),
	append(F1Name,Suff,FName),
	!.
removeSuffixChars(FName,F1Name) :-
	append("_ans",_,Suff),
	append(F1Name,Suff,FName),
	!.
removeSuffixChars(FName,FName).


wideningPoints_file(ResultDir, F_WidenPoints) :-
	path_concat(ResultDir, 'widenpoints', F_WidenPoints).

traceTerm_file(ResultDir, F_Traceterm) :-
	path_concat(ResultDir, 'traceterm.out', F_Traceterm).

threshold_file(ResultDir, F_Threshold) :-
	path_concat(ResultDir, 'wut.props', F_Threshold).

int_file(ResultDir, F, F_Int) :-
	atom_concat(F, '.int.pl', F_Int0),
	path_concat(ResultDir, F_Int0, F_Int).

qa_file(ResultDir, F, F_QA) :-
	atom_concat(F, '.qa.pl', F_QA0),
	path_concat(ResultDir, F_QA0, F_QA).

qa_cpa_file(ResultDir, F, QA_CPA) :-
	atom_concat(F, '.qa.pl.cha.pl', QA_CPA0),
	path_concat(ResultDir, QA_CPA0, QA_CPA).
cpa_file(ResultDir, F, CPA) :-
	atom_concat(F, '.cha.pl', CPA0),
	path_concat(ResultDir, CPA0, CPA).

sp_file(ResultDir, F, F_SP) :-
	atom_concat(F, '.pe.pl', F_SP0),
	path_concat(ResultDir, F_SP0, F_SP).

fta_file(ResultDir, F, F_FTA) :-
	atom_concat(F, '.fta.pl', F_FTA0),
	path_concat(ResultDir, F_FTA0, F_FTA).

dfta_file(ResultDir, F, F_DFTA) :-
	atom_concat(F, '.dfta.pl', F_DFTA0),
	path_concat(ResultDir, F_DFTA0, F_DFTA).

split_file(ResultDir, F, F_SPLIT) :-
	atom_concat(F, '.split.pl', F_SPLIT0),
	path_concat(ResultDir, F_SPLIT0, F_SPLIT).

refine_file(ResultDir, F, F_REFINE) :-
	atom_concat(F, '.refine.pl', F_REFINE0),
	path_concat(ResultDir, F_REFINE0, F_REFINE).

createTmpFilePP(ResultDir, F, F_Int, F_QA, QA_CPA, F_CPA, F_SP, F_WidenPoints, F_Traceterm, F_Threshold):-
	int_file(ResultDir, F, F_Int),
	qa_file(ResultDir, F, F_QA),
	qa_cpa_file(ResultDir,F, QA_CPA),
	sp_file(ResultDir, F, F_SP),
	wideningPoints_file(ResultDir, F_WidenPoints),
	traceTerm_file(ResultDir, F_Traceterm),
	threshold_file(ResultDir, F_Threshold),
	cpa_file(ResultDir, F, F_CPA).

createTmpFileRef(ResultDir, F, F_FTA, F_DFTA, F_SPLIT, F_REFINE):-
	fta_file(ResultDir, F, F_FTA),
	dfta_file(ResultDir, F, F_DFTA),
	split_file(ResultDir, F, F_SPLIT),
	refine_file(ResultDir, F, F_REFINE).
