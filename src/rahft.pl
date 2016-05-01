:- module(rahft, [main/1], []).

% Input: a set of Horn clauses or program P
% Output: safe/unsafe
% 
% Given P it checks whether P has a model. If P has a model it return
% "safe"; if P has no model it returns "unsafe". It uses abstraction
% refinement algorithm for checking the existence of a model. For
% details: http://akira.ruc.dk/~kafle/publications/comlan-15

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

:- use_module(raf, [main/1]).
:- use_module(counterExample, [counterExample/2]).
:- use_module(insertProps, [main/1]).
:- use_module(splitVersions, [main/1]).
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
:- include(chclibs(messages)).

% stores output of the tool
logfile('result.txt').

% (debug)
% go :-
% 	rahft:main(['/Users/kafle/Desktop/RAHFT/examples/running.nts.pl']).

% ---------------------------------------------------------------------------
% Main
% ---------------------------------------------------------------------------

:- data flag/1.
:- data opt_debug_temps/0.

% printing output of RAHFT
displayHelpMenu:-
	help_msg(Str),
	format(user_error, "~s~n", [Str]).

help_msg(
"Usage: rahft <prog> [<Options>]

Options:
 -help        display this help menu
 -v           verbose
 -raf         enable redundant argument filtering
 -splitvers   enable splitting of disjoint clauses
 -int         uses interpolant automaton for trace generalisation during refinement
 -model       show model
 -array       enable array constraints
 -sp          only horn specialization
 -itr N       limit abstract refine iterations 

 -debug-temps    keep files for intermediate passes (debug)
").

recognised_option('-help',  help, []).
recognised_option('-model', model, []).
recognised_option('-v', verbose, []).
recognised_option('-raf', raf, []).
recognised_option('-splitvers', splitvers, []).
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
	retractall_fact(flag(_)).

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
	    assertz_fact(flag(array))
	; true
	),
	( member(raf, Options) ->
	    assertz_fact(flag(raf))
	; true
	),
	( member(splitvers, Options) ->
	    assertz_fact(flag(splitvers))
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
	retractall_fact(flag(verbose)),
	( member(verbose, Options) ->
	    assertz_fact(flag(verbose))
	; true
	),
	applyRAHFT(Prog, WithInterpolant, ShowModel, Bounded).

% ---------------------------------------------------------------------------
% Horn clause pre-processing
% ---------------------------------------------------------------------------

preProcessHorn(Prog, F_Int, F_QA, QACPA, F_WidenPoints, F_Threshold, OutputFile):-
	verbose_opts(VerbOpts),
	( flag(array) ->
	    % TODO:{arrays(1): "preprocess"}
	    %   for each clause in P, apply 
	    %    - constraint replacement algorithm of [1] (Section 5.2, page 338 RR1-WR3);
	    %     step 1 of [1] (Section 5.3, page 344) // Note: This is “delete all write constraints”
	    %   producing P2;
	    verbose_message(['Preprocessing array constraints']),
	    Prog2 = Prog,
	    % TODO:{arrays(2): "integer_program"}
	    verbose_message(['Getting integer program']),
	    Prog2Int = F_Int,
	    integerProgram:main(['-prg', Prog2, '-o', Prog2Int])
	; Prog2 = Prog,
	  Prog2Int = Prog2
	),
	verbose_message(['Computing query-answer transformation']),
	qa:main([Prog2Int, '-query', 'false', '-ans',  '-o', F_QA]),
	verbose_message(['Computing widening thresholds for QA program']),
	thresholds1:main(['-prg', F_QA, '-o', F_Threshold]),
	verbose_message(['Analyse QA program']),
	cpascc:main(['-prg', F_QA, '-withwut', 'bounded', '-wfunc', 'h79', '-widenpoints',F_WidenPoints,'-threshold', F_Threshold, '-o', QACPA|VerbOpts]),
	% NOTE: Using Prog2, not Prog2Int!
	( flag(array) -> % TODO:{arrays} add more control over strengthening
	    InsertPropsOpts = ['-array']
	; InsertPropsOpts = []
	),
	( flag(splitvers) ->
	    atom_concat(OutputFile, '.insprops.pl', F_InsProps)
	; F_InsProps = OutputFile
	),
	insertProps:main(['-prg', Prog2, '-props', QACPA, '-o', F_InsProps|InsertPropsOpts]),
	( flag(splitvers) ->
	    verbose_message(['Spliting disjoint versions']),
	    splitVersions:main(['-prg', F_InsProps, '-o', OutputFile])
	; true
	).

% ---------------------------------------------------------------------------
% Analysis using CPA
% ---------------------------------------------------------------------------

verifyCPA(Prog, F_Int, F_QA, QACPA, F_CPA, OutputFile, F_WidenPoints, F_Traceterm, F_Threshold,Result) :-
	verbose_opts(VerbOpts),
	preProcessHorn(Prog, F_Int, F_QA, QACPA, F_WidenPoints, F_Threshold, OutputFile),
	verbose_message(['Checking for the presence of false clauses']),
	checkFalseInFile:checkForFalse(OutputFile, Result1),
	( Result1=safe -> % no (false :- ...)
	    % TODO:{arrays} "if there is no trace for false in A_P' then return safe" (is it equivalent?)
	    Result=safe
	; 
	    ( flag(array) ->
	        % TODO:{arrays(4) - "abstract_analyse"}
	        % for each clause in P, apply
	        %     constraint generalisation algorithm (step 2-3) of [1] (Section 5.3, page 344)
	        %   producing P2;
	        verbose_message(['Generalizing constraint arrays']),
		OutputFile2 = OutputFile,
	        % TODO:{arrays(2): "integer_program"}
	        %   compute integer program ProgInt (use it in qa,thresholds1,cpascc; but not in insertProps)
	        verbose_message(['Getting integer program']),
	        atom_concat(OutputFile2, '.int.pl', OutputFile2Int),
	        integerProgram:main(['-prg', OutputFile2, '-o', OutputFile2Int])
	    ; OutputFile2 = OutputFile,
	      OutputFile2Int = OutputFile2
	    ),
	    %
	    verbose_message(['Computing widening thresholds for PE program']),
	    thresholds1:main(['-prg', OutputFile2Int, '-o', F_Threshold]),
	    verbose_message(['Analyse PE program']),
	    cpascc:main(['-prg', OutputFile2Int, '-withwut', 'bounded', '-wfunc', 'h79', '-widenpoints',F_WidenPoints, '-threshold', F_Threshold, '-cex', F_Traceterm, '-o', F_CPA]),
	    verbose_message(['Analysing counterexample']),
	    ( flag(array) ->
	        % TODO:{arrays(5) - in counterExample.pl} "need extension to the theory of arrays"
	        write(user_error, 'TODO: use arrays in cex'), nl(user_error),
	        counterExample:counterExample([OutputFile, F_Traceterm|VerbOpts], Result)
	    ; counterExample:counterExample([OutputFile, F_Traceterm|VerbOpts], Result)
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

refineHorn(F_SP, F_FTA, F_DFTA, F_SPLIT, F_TRACETERM, F_REFINE, WithInterpolant):-
	verbose_opts(VerbOpts),
        verbose_message(['Generate FTA from program and error trace']),
        genfta:main(['-prg', F_SP, '-trace', F_TRACETERM, '-o', F_FTA]),
        ( WithInterpolant=yes ->
            verbose_message(['Computing interpolant automaton from an error trace']),
            interpolantAutomaton:main(['-prg', F_SP,  '-trace',  F_TRACETERM, '-o',  F_FTA|VerbOpts])
	; true
        ),
	%
	determinise_jar(DeterminiseJar),
	( flag(verbose) ->
	    DeterminiseOpts = []
	; DeterminiseOpts = [stdout(null)]
	),
	process_call(path(java), ['-jar', DeterminiseJar, F_FTA, '-nodc', '-show', '-o', F_DFTA], DeterminiseOpts),
	%
        verbose_message(['Find disjoint clauses']),
        splitClauseIds:main(['-prg', F_SP, '-o', F_SPLIT]),
        verbose_message(['Refining using DFTA']),
        ftaRefine:main(['-prg', F_SP, '-fta', F_DFTA, '-split', F_SPLIT, F_SPLIT, '-o', F_REFINE]).

% ---------------------------------------------------------------------------
% printing output of RAHFT
% ---------------------------------------------------------------------------

printRahftOutput(LogS, Prog, Safety, Iteration, InterpolantOption, Time):-
	printRahftOutput_(LogS, Prog, Safety, Iteration, InterpolantOption, Time),
	printRahftOutput_(user_output, Prog, Safety, Iteration, InterpolantOption, Time).


%[solver(rahft), program('addition.nts.pl'), safety(safe), iteration(0), time(129.487,ms), opt('-int')].
printRahftOutput_(LogS, Prog, Safety, Iteration, InterpolantOption, Time):-
	format(LogS, '[solver(rahft), ', []),
	format(LogS, 'program(~q), ', [Prog]),
	format(LogS, 'safety(~w), ', [Safety]),
	format(LogS, 'iteration(~w), ', [Iteration]),

    (InterpolantOption=yes ->
        I = '-int'
    ; I = '-noint'),

    format(LogS, 'option(~q), ', [I]),
	format(LogS, 'time(~w, ms)]. ~n', [Time]).

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
	( flag(raf) ->
	    verbose_message(['Redundant argument filtering']),
	    % (it may increase precision or even solvability of some problems)
	    raf_file(ResultDir, F, F_Raf),
	    raf:main([Prog1, false, F_Raf]),
	    Prog2 = F_Raf
	; Prog2 = Prog1
	),
	abstract_refine(Bounded, LogS,  Prog2, K, Result, K1, WithInterpolant, F_Int, F_QA, QA_CPA, F_CPA, F_SP,F_WidenPoints, F_TRACETERM, F_THRESHOLD, F_FTA, F_DFTA, F_SPLIT, F_REFINE),
	statistics(runtime,[END|_]),
	( ShowModel = no -> true
	; ( Result=safe ->
              showModel(QA_CPA, F_CPA, Prog2, F_REFINE)
	  ; ( Result=unsafe ->
	        write('There is no model since the program is unsafe'), nl
            ; write('We do not know if there exists a model for the program'), nl
            )
	  )
	),
	DIFF is END - START,
	path_basename(Prog1, F),
	printRahftOutput(LogS,F, Result, K1, WithInterpolant, DIFF),
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
	    verbose_message(['the program is safe'])
	; Ret1 = unsafe ->
	    Result= Ret1,
	    K2 = K,
	    verbose_message(['the program is unsafe'])
	; % refinement with FTA
	  refineHorn(F_SP, F_FTA, F_DFTA,  F_SPLIT, F_TRACETERM, F_REFINE, WithInterpolant),
	  K1 is K + 1,
	  abstract_refine(Bounded, LogS,  F_REFINE, K1, Result, K2, WithInterpolant, F_Int, F_QA, QA_CPA, F_CPA, F_SP,F_WidenPoints, F_TRACETERM, F_THRESHOLD, F_FTA, F_DFTA, F_SPLIT, F_REFINE)
	).

hornSpecialise(Prog, OutputFile):-
	% TODO: Not using '-raf' option, fix
	prepare_resultdir(Prog, ResultDir),
	path_basename(Prog, F),
	createTmpFilePP(ResultDir, F, F_Int, F_QA, QA_CPA,_,_,F_WidenPoints, _, F_THRESHOLD),
	statistics(runtime,[START|_]),
	preProcessHorn(Prog, F_Int, F_QA, QA_CPA, F_WidenPoints, F_THRESHOLD, OutputFile),
	statistics(runtime,[END|_]),
	DIFF is END - START,
	path_basename(Prog, F),
	verbose_message(['Total time: ', DIFF, ' ms.']),
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


raf_file(ResultDir, F, F_Raf) :-
	atom_concat(F, '.raf.pl', F_Raf0),
	path_concat(ResultDir, F_Raf0, F_Raf).

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
