#!/bin/bash
set -e
set -u


PROGNAME=$(basename $0)
function fail
{
    echo "${PROGNAME}: Test failed." 1>&2
    exit 1
}

TESTDIR=$(dirname $0)
TESTFILE=$TESTDIR/example.xml
PROG=$TESTDIR/../dist/build/xml-to-json/xml-to-json
DIFF=diff

$PROG $TESTFILE         | $DIFF - $TESTDIR/expected_default.js || fail
$PROG -t Test $TESTFILE | $DIFF - $TESTDIR/expected_t_Test.js  || fail
$PROG -s $TESTFILE      | $DIFF - $TESTDIR/expected_s.js       || fail
$PROG -a $TESTFILE      | $DIFF - $TESTDIR/expected_a.js       || fail
$PROG -sam $TESTFILE    | $DIFF - $TESTDIR/expected_sam.js     || fail
$PROG --no-collapse-text '.*More.*' $TESTFILE    | $DIFF - $TESTDIR/expected_no_collapse_text_pattern.js  || fail

echo "All tests passed."
