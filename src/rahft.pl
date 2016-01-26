:- module(rahft, [main/1], []).

% Solves a non-linear Horn clause using a linear solver.
% Input: a set of Horn clauses
% Output: safe if the program is solved else unknown if it is not solved

:- use_module(library(format), [format/2, format/3]).
:- use_module(library(system_extra), [mkpath/1]).
:- use_module(library(system)). % mktemp_in_tmp is available here
:- use_module(library(pathnames), [path_basename/2, path_concat/3]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(prolog_sys), [statistics/2]).

:- use_module(thresholds1, [main/1]).
:- use_module(counterExample, [main/1]).
:- use_module(cpascc, [main/1]).
:- use_module(qa, [main/1]).
:- use_module(insertProps, [main/1]).
:- use_module(genfta, [main/1]).
:- use_module(splitClauseIds, [main/1]).
:- use_module(ftaRefine, [main/1]).
:- use_module(interpolantAutomaton, [main/1]).
:- use_module(checkFalseInFile, [checkForFalse/2]).

:- use_module(library(write)).

:- use_module(library(source_tree), [remove_dir/1]).

:- use_module(library(process)). % invoking external processes

% stores output of the tool
logfile('result.txt').

go :-
	rahft:main(['/Users/kafle/Desktop/RAHFT/examples/running.nts.pl']).

% ---------------------------------------------------------------------------
% Horn clause pre-processing
% ---------------------------------------------------------------------------

preProcessHorn(Prog, QAFile, QACPA, F_WidenPoints, F_Threshold, OutputFile):-
	format("Computing query-answer transformation ~n", []),
	qa:main([ Prog, '-query', 'false', '-ans',  '-o', QAFile]),
	format("Computing widening thresholds for QA program~n", []),
	thresholds1:main(['-prg', QAFile, '-o', F_Threshold]),
	format("Analyse QA program~n", []),
	cpascc:main(['-prg', QAFile, '-withwut', 'bounded', '-wfunc', 'h79', '-widenpoints',F_WidenPoints,'-threshold', F_Threshold, '-o', QACPA]),
	insertProps:main([ '-prg', Prog, '-props', QACPA,   '-o', OutputFile]).

% ---------------------------------------------------------------------------
% Analysis using CPA
% ---------------------------------------------------------------------------

verifyCPA(Prog, QAFile, QACPA, OutputFile, F_WidenPoints, F_Traceterm, F_Threshold,Result) :-
	preProcessHorn(Prog, QAFile, QACPA, F_WidenPoints, F_Threshold, OutputFile),
	format("Checking for the presence of false clauses~n", []),
	checkFalseInFile:checkForFalse(OutputFile, Result1),
	( Result1=safe ->
	    Result=safe
	;
	    format("Computing widening thresholds for PE program~n", []),
	    thresholds1:main(['-prg', OutputFile, '-o', F_Threshold]),
	    format("Analyse PE program~n", []),
	    cpascc:main(['-prg', OutputFile, '-withwut', 'bounded', '-wfunc', 'h79', '-widenpoints',F_WidenPoints, '-threshold', F_Threshold, '-cex', F_Traceterm]),
	    format("Analysing  counterexample~n", []),
	    counterExample:main([OutputFile, F_Traceterm, Result])
	).

% ---------------------------------------------------------------------------
% Refinement
% ---------------------------------------------------------------------------

refineHorn(F_SP, F_FTA, F_DFTA, F_SP, F_SPLIT, F_TRACETERM, F_REFINE, WithInterpolant):-
        format( "Generate FTA from program and error trace~n", []),
        genfta:main(['-prg', F_SP, '-trace', F_TRACETERM, '-o', F_FTA]),
        ( WithInterpolant=int ->
            format( "Computing interpolant automaton from an error trace~n", []),
            interpolantAutomaton:main(['-prg', F_SP,  '-trace',  F_TRACETERM, '-o',  F_FTA])
	; true
        ),
	process_call('/usr/bin/java', ['-jar', '/Users/kafle/Desktop/RAHFT/src/determinise.jar', F_FTA, '-nodc', '-show', '-o', F_DFTA], []),
        format( "Find disjoint clauses ~n", []),
        splitClauseIds:main(['-prg', F_SP, '-o', F_SPLIT]),
        format( "Refining using DFTA ~n", []),
        ftaRefine:main(['-prg', F_SP, '-fta', F_DFTA, '-split', F_SPLIT, F_SPLIT, '-o', F_REFINE]).

% ---------------------------------------------------------------------------
% printing output of RAHFT
% ---------------------------------------------------------------------------

printRahftOutput(LogS, Prog, Safety, Iteration, Time):-
	format(LogS, 'RAHFT: {', []),
	format(LogS, 'Program: ~w, ', [Prog]),
	format(LogS, 'Safety: ~w, ', [Safety]),
	format(LogS, 'Iteration: ~w, ', [Iteration]),
	format(LogS, 'Time: ~w millisecs.} ~n', [Time]).

remove_resultdir(ResultDir) :-
	( file_exists(ResultDir) -> remove_dir(ResultDir) ; true ).

% ---------------------------------------------------------------------------
% printing output of RAHFT
% ---------------------------------------------------------------------------

displayHelpMenu:-
	%format( 'Error: Input File missing ~n', []),
	%format( 'Usage: rahft <Input File> <Option>* ~n', []),
	format(user_error, "~n Usage: rahft <prog> <Option>* ~n~n", []),
	format( 'Option: ~n', []),
	format( '-int:  uses interpolant automaton for trace generalisation during refinement~n', []),
	format( '-help: display this help menu~n', []).

% ---------------------------------------------------------------------------
% main procedure RAHFT
% ---------------------------------------------------------------------------

main(['-help']) :- !,
	displayHelpMenu.
main([Prog]) :- !,
	applyRAHFT(Prog, '$NOINTERPOLANTAUT').
main([Prog,'-int']) :- !,
	applyRAHFT(Prog, 'int').
main(_) :- !,
	displayHelpMenu.

applyRAHFT(Prog1, WithInterpolant) :-
	logfile(LogFile),
	open(LogFile, append, LogS),
	%creating temporary directory for intermediate files
	%mktemp_in_tmp('linearsolveHorn', ResultDir),
	atom_concat(Prog1, '_output', ResultDir),
	mkpath(ResultDir),
	format( "temp dir ~w~n", [ResultDir]),
	K = 0,
	statistics(runtime,[START|_]),
	loop(LogS, ResultDir, Prog1, K, Result, K1, WithInterpolant),
	statistics(runtime,[END|_]),
	DIFF is END - START,
	path_basename(Prog1, F),
	printRahftOutput(LogS,F, Result, K1, DIFF),
	%format(LogS, "#####################################################################~n", []),
	%remove the directory of intermediate files
	remove_resultdir(ResultDir),
	close(LogS).

loop(LogS, ResultDir, Prog1,  K, Result, K2, WithInterpolant) :-
	path_basename(Prog1, F),
	createTmpFilePP(ResultDir, F, F_QA, QA_CPA,F_SP,F_WidenPoints, F_TRACETERM, F_THRESHOLD),
	verifyCPA(Prog1, F_QA, QA_CPA, F_SP, F_WidenPoints, F_TRACETERM, F_THRESHOLD, Ret1),
	( Ret1 = safe ->
	    Result = Ret1,
	    K2 = K,
	    format("the  program is safe~n", [])
	; Ret1 = unsafe ->
	    Result= Ret1,
	    K2 = K,
	    format("the  program is unsafe~n", [])
	; % refinement with FTA
	  createTmpFileRef(ResultDir, F, F_FTA, F_DFTA, F_SPLIT, F_REFINE),
	  refineHorn(F_SP, F_FTA, F_DFTA, F_SP, F_SPLIT, F_TRACETERM, F_REFINE, WithInterpolant),
	  K1 is K + 1,
	  loop(LogS, ResultDir, F_REFINE, K1, Result, K2, WithInterpolant)
	).

wideningPoints_file(ResultDir, F_WidenPoints) :-
	path_concat(ResultDir, 'widenpoints', F_WidenPoints).

traceTerm_file(ResultDir, F_Traceterm) :-
	path_concat(ResultDir, 'traceterm.out', F_Traceterm).

threshold_file(ResultDir, F_Threshold) :-
	path_concat(ResultDir, 'wut.props', F_Threshold).

qa_file(ResultDir, F, F_QA) :-
	atom_concat(F, '.qa.pl', F_QA0),
	path_concat(ResultDir, F_QA0, F_QA).

qa_cpa_file(ResultDir, F, QA_CPA) :-
	atom_concat(F, '.qa.pl.cha.pl', QA_CPA0),
	path_concat(ResultDir, QA_CPA0, QA_CPA).

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

createTmpFilePP(ResultDir, F, F_QA, QA_CPA,F_SP, F_WidenPoints, F_Traceterm, F_Threshold):-
	qa_file(ResultDir, F, F_QA),
	qa_cpa_file(ResultDir,F, QA_CPA),
	sp_file(ResultDir, F, F_SP),
	wideningPoints_file(ResultDir, F_WidenPoints),
	traceTerm_file(ResultDir, F_Traceterm),
	threshold_file(ResultDir, F_Threshold) .

createTmpFileRef(ResultDir, F, F_FTA, F_DFTA, F_SPLIT, F_REFINE):-
	fta_file(ResultDir, F, F_FTA),
	dfta_file(ResultDir, F, F_DFTA),
	split_file(ResultDir, F, F_SPLIT),
	refine_file(ResultDir, F, F_REFINE).
