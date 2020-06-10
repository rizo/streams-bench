#!/usr/bin/env bash

set -u
set -e
set -o pipefail

for switch in 4.10.0+flambda
do
  echo "Running benchmarks in switch $switch..."
  for bench in all all_no_flat_map all_no_take fold map filter flat_map take
  do
    export STREAMS_BENCHMARK=$bench
    for len in 10 100 1000 10000 100000 1000000
    do
      export STREAMS_LENGTH="$len"
      export STREAMS_LIMIT="$(($len/2))"
      opam exec --switch="$switch" -- dune exec -- ./src/Cmd_core_bench.exe -quota 10 -ascii
    done
  done
done
