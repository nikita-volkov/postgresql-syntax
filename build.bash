#!/bin/bash
set -eo pipefail


build_ghc_options="-j +RTS -A128m -n2m -N -RTS -fwarn-incomplete-patterns"


function if_then {
  if [[ -n "$1" ]]; then
    echo "$2"
  fi
}

function format {
  ormolu --mode inplace -ce \
  $(find . -name "*.hs" \
    -not -path "./*.stack-work/*" \
    -not -path "./.git/*")
}

function build_all {
  stack build \
  --ghc-options "$build_ghc_options" \
  --fast \
  --test --no-run-tests \
  --bench --no-run-benchmarks
}

function haddock {
  stack haddock \
  --work-dir ".haddock.stack-work"
}

function test {
  stack build \
  --ghc-options "$build_ghc_options" \
  --test \
  --fast
}

function bench {

  bench_work_dir=".bench.stack-work"
  pattern="$1"

  stack \
  --work-dir "$bench_work_dir" \
  build \
  --ghc-options "-O2 -rtsopts $build_ghc_options" \
  --bench \
  --ba "-m pattern \"$pattern\" +RTS -A128m -n2m -RTS"

}


format
build_all
