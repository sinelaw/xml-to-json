#!/bin/bash
set -e
set -u

TESTDIR=$(dirname $0)
TESTFILE=$TESTDIR/example.xml
PROG=$TESTDIR/../dist/build/xml-to-json/xml-to-json
FAIL='echo Test Failed'
$PROG $TESTFILE         | diff - $TESTDIR/expected_default.js || $FAIL
$PROG -t Test $TESTFILE | diff - $TESTDIR/expected_t_Test.js  || $FAIL
$PROG -s $TESTFILE      | diff - $TESTDIR/expected_s.js       || $FAIL
$PROG -a $TESTFILE      | diff - $TESTDIR/expected_a.js       || $FAIL
$PROG -sam $TESTFILE    | diff - $TESTDIR/expected_sam.js     || $FAIL
$PROG --no-collapse-text $TESTFILE    | diff - $TESTDIR/expected_no_collapse_text.js  || $FAIL

echo "All tests passed."