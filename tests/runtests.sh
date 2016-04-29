#!/bin/bash

# TODO: Needs a value for CIAOPATH

# Exit immediately if a simple command exits with a non-zero status
set -e

# Physical directory where the script is located
_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P)

set -e

# ---------------------------------------------------------------------------

if which gtimeout > /dev/null 2>&1; then
    timeoutcmd=gtimeout
elif which timeout > /dev/null 2>&1; then
    timeoutcmd=timeout
else
    timeoutcmd=notimeout
fi

function notimeout() {
    shift
    "$@"
}

# ---------------------------------------------------------------------------

solver=$CIAOPATH/build/bin/rahft
# TODO: try with -int, etc.

# Write output for a timeout
function solver_timeout() {
    local f=`basename $1`
    echo "[solver(rahft), program('""$f""'), timeout($hard_timeout)]."
    echo "[solver(rahft), program('""$f""'), timeout($hard_timeout)]." >> $solver_res
}
#   
solveropts=
if [ $# -gt 0 ]; then
    solveropts="$*"
fi

testdir="../examples"
tests="\
addition.nts.pl \
bfprt.nts.pl \
binarysearch.nts.pl \
countZero.nts.pl \
fib.pl \
identity.nts.pl \
merge.nts.pl \
palindrome.nts.pl \
parity.nts.pl \
remainder.nts.pl \
running.nts.pl"

solver_res="result.txt"

hard_timeout=900 # 15 minutes timeout

cd "$_base"

rm -f "$solver_res"
for i in $tests; do
    echo "### SOLVING $i (opts: $solveropts) ###"
    set +e
    $timeoutcmd "$hard_timeout" $solver "$testdir/$i" $solveropts
    err=$?
    set -e
    if [ $err -eq 124 ]; then # Timeout
	# Write output for timeout
	solver_timeout "$testdir/$i" $solveropts
    fi
done

if diff <(sed 's/time(.*)/time/g' "$solver_res") \
	<(sed 's/time(.*)/time/g' "$solver_res"-good); then
    printf "\nTESTS SEEMS OK\n"
else
    printf "\nTESTS DIFFER, SOMETHING MAY BE WRONG\n"
fi

