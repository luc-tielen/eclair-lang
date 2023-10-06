#!/bin/bash

grep -rE "(fdescribe|fit)" tests/eclair

if [ "$?" == "0" ]; then
  echo "Found disabled tests (marked with fdescribe / fit), aborting!"
  exit 1
fi

grep -rE "(\sxit|pending)" tests/eclair/Test
if [ "$?" == "0" ]; then
  echo "Found pending tests, aborting!"
  exit 1
fi

echo "All tests are enabled!"
exit 0
