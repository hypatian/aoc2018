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
    def dups = (seen zip sums).filter({ case (a, b) => a contains b }).map({ case (_, sum) => sum })
```

## Building and Running

You'll need a Java runtime installed. You'll also need the
[Mill](http://www.lihaoyi.com/mill/) build tool installed. Once you
have these, you should be able to build and run the individual days'
code like `mill day<n>.run`, or you can run all of them with `mill
_.run`.
