#!/bin/sh

set -e

TEST_CASE=$1
BINARY=$(echo $TEST_CASE | sed 's/silly-ml$/bin/')
EXPECTED_OUTPUT=$(echo $TEST_CASE | sed 's/silly-ml$/expected/')
EXIT_CODE=$(echo $TEST_CASE | sed 's/silly-ml$/exit-code/')
if [ -f "$EXIT_CODE" ]; then
    EXPECTED_EXIT_CODE=$(cat $EXIT_CODE)
else
    EXPECTED_EXIT_CODE=0
fi

COMPILER=${COMPILER-./compiler.native}

$COMPILER -L=runtime -o $BINARY $TEST_CASE

ACTUAL_OUTPUT=$(mktemp $TEST_CASE.XXXXXX)
trap "rm -f $ACTUAL_OUTPUT" EXIT

set +e
./$BINARY 2>&1 > $ACTUAL_OUTPUT
EXIT_CODE=$?
set -e

if [ $EXIT_CODE -ne $EXPECTED_EXIT_CODE ]; then
    echo "exit code mismatch: actual=$EXIT_CODE expected=$EXPECTED_EXIT_CODE"
    exit 1;
fi

if [ -f "$EXPECTED_OUTPUT" ]; then
    diff $ACTUAL_OUTPUT $EXPECTED_OUTPUT
fi
