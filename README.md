# Advent of Code 2018

## Notes

The dialect of Scala in use here is [Dotty](http://dotty.epfl.ch/), which is
the test-bed compiler for ideas for Scala 3. This is selected by the use of
the "0.11.0-RC1" compiler version in `build.sc`. The main new feature present
so far is the [automatic tupling of function parameters](http://dotty.epfl.ch/docs/reference/auto-parameter-tupling.html)
which means I can write things like this:

```scala
    def seen = sums.scanLeft(Set.empty)(_ + _)
    def dups = (seen zip sums).filter(_ contains _).map((_, sum) => sum)
```

instead of things like this:

```scala
    def seen = sums.scanLeft(Set.empty)({ case (a, b) => a + b })
    def dups = (seen zip sums).filter(_ contains _).map({ case (_, sum) => sum })
```

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

## Day 2

```sh
mill day2.problem1.run day2/input.txt
mill day2.problem2.run day2/input.txt
```
