% Manifest file for RAHFT
bundle_name('RAHFT').
bundle_packname('RAHFT').
bundle_requires([
    core,
    chclibs,
    ciao_yices
]).
bundle_alias_paths([
    rahft = 'src'
]).


