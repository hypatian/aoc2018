# Advent of Code 2018

## Building and Running

You'll need a Java runtime installed. You'll also need the
[Mill](http://www.lihaoyi.com/mill/) build tool installed. Once you have
these, you should be able to build and run the individual days' code
like `mill day<n>.problem<n>.run [OPTIONS]`. See below for how each
day's problems should be run.

You may wish to redirect standard error to `/dev/null` to avoid output
from the build tool.

## Day 1

```sh
mill day1.problem1.run day1/input.txt
mill day1.problem2.run day1/input.txt
```
