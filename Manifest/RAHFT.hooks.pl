:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for RAHFT").

'$builder_hook'(desc_name('RAHFT')).

% ============================================================================

:- use_module(ciaobld(ciaoc_aux), [build_libs/2]).

'$builder_hook'(build_libraries) :-
	build_libs('RAHFT', 'src').

'$builder_hook'(build_bin) :-
	bundleitem_do(rahftcl, 'RAHFT', build_nodocs).

% TODO: just say cmd('cmds/rahftcl', [...])
'$builder_hook'(rahftcl:item_def( 
    cmds_list('RAHFT', bundle_src('RAHFT')/'src', [
        'rahft'-[
          output='rahft', % (executable will be called 'rahft')
	  plexe,
	  final_ciaoc
	]
    ]))).

'$builder_hook'(install) :- bundleitem_do(only_global_ins(~rahft_desc), 'RAHFT', install).

'$builder_hook'(uninstall) :- bundleitem_do(only_global_ins(~rahft_desc), 'RAHFT', uninstall).

rahft_desc := [
  rahftcl,
  lib('RAHFT', 'src')
].
