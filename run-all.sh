#!/bin/sh

function run {
  dn="$1"
  pn="$2"
  shift 2
  echo mill "day${dn}.problem${pn}.run" "$@"
  mill "day${dn}.problem${pn}.run" "$@"
}

run 1 1 day1/input.txt
run 1 2 day1/input.txt
