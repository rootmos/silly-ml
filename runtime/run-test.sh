#!/bin/sh

TEST_CASE=$1

EXPECTED=$(echo $TEST_CASE | sed 's/bin$/expected/')
ENV=$(echo $TEST_CASE | sed 's/bin$/env/')

if [ ! -f $ENV ]; then
    ENV=/dev/null
fi

if [ -f $EXPECTED ]; then
    env $(cat $ENV | xargs) ./$TEST_CASE | diff $EXPECTED -
else
    env $(cat $ENV | xargs) ./$TEST_CASE
fi
