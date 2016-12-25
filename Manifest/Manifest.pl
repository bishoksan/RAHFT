:- bundle('RAHFT').
version('1.0').
depends([
    core,
    chclibs,
    'github.com/jfmc/ciao_yices'
]).
alias_paths([
    rahft = 'src'
]).
lib('src').
cmd('src/rahft').
