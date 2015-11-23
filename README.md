#!/bin/sh

# Abstraction refinement tool for Verifying  a set of Horn clauses with respect to the integrity constraints#
# Input: a set of Horn clauses#
# Output: safe, unsafe#

###########################################################################

###########################################################################
#please change direction to HOME_RAHFT  before running.

HOME_RAHFT="/Users/kafle/Desktop/RAHFT/src" # home to all src  files


###########################################################################



###########################################################################

# $1 is program name,
# $2 intermediate result directory



function verify_pepm15(){
    local prog=$1
    local f=`basename $prog`
    local resultdir=$2
    echo "Performing query transformation"
    $HOME_RAHFT/qa $prog -query false -ans -o $resultdir/$f.qa.pl
    echo "Computing widening thresholds"
    $HOME_RAHFT/thresholds -prg $resultdir/$f.qa.pl -o wut.props
    echo "Computing convex polyhedron approximation"
    $HOME_RAHFT/cpascc -prg $resultdir/$f.qa.pl  -withwut bounded -wfunc h79 -o $resultdir/$f.qa.cha.pl
    echo "Specialise clauses"
    $HOME_RAHFT/insertProps -prg $prog -props $resultdir/$f.qa.cha.pl -o $resultdir/$f.pe.pl
    $HOME_RAHFT/splitVersions -prg $resultdir/$f.pe.pl -o $resultdir/$f.split.pl
    echo "Computing widening thresholds for specialised program"
    $HOME_RAHFT/thresholds -prg $resultdir/$f.split.pl -o wut.props
    echo "Analyse specialised program"
    $HOME_RAHFT/cha -prg $resultdir/$f.split.pl  -withwut bounded -wfunc h79 -o $resultdir/$f.pe.cha.pl
    echo "Checking safety"
    $HOME_RAHFT/checkSafety $resultdir/$f.pe.cha.pl
    echo "Checking feasibility of trace"
    # return status
    # 0 = safe
    # 0 = unsafe (see traceterm.out for error trace)
    # 1 = spurious error
    $HOME_RAHFT/counterExample $resultdir/$f.split.pl
    # return the command form counterExample
    return $?

}



###########################################################################


iteration=0
resultdir=$1_output

if (test ! -d $resultdir) then
mkdir $resultdir
fi

prog=$1
f=`basename $prog`

echo " verifying $f"

verify_pepm15 $prog $resultdir

do

# REFINEMENT STEP
echo "Generate FTA from program and error trace"
$HOME_RAHFT/genfta -prg $resultdir/$f.split.pl -o $resultdir/$f.fta.pl
java -jar $HOME_RAHFT/determinise.jar $resultdir/$f.fta.pl -nodc -show -o $resultdir/$f.dfta.pl
#echo "Find disjoint clauses"
#$HOME_SPLIT/splitClauseIds -prg $resultdir/$f.pe.pl -o $resultdir/$f.split.pl
echo "Refining using DFTA"
$HOME_RAHFT/ftaRefine -prg $resultdir/$f.split.pl -fta $resultdir/$f.dfta.pl -o $resultdir/$f.refine.pl

prog=$resultdir/$f.refine.pl
iteration=`expr $iteration \+ 1`
#echo ITERATION = $iteration

done

echo ITERATION = $iteration


