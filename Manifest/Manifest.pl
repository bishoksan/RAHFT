:- bundle('RAHFT').
version('1.0').
depends([
    core-[version>='1.16'],
    chclibs,
    ciao_ppl,
    'github.com/jfmc/ciao_yices'
]).
alias_paths([
    rahft = 'src'
]).
lib('src').
cmd('src/rahft').
