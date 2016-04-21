# calcƶ

> a command-line calculator written in scala

This is a little program i wrote to teach myself [scalaz](https://github.com/scalaz/scalaz).

Currently using:
* The IO monad
* ValidationNel
* Show

You can compile the program using `sbt compile`, run it using `sbt run` for
development you can use `sbt ~run` and type `exit` whenever you want to recompile.

**calcƶ** reads a line from stdin and tries to form an expression tree.
If that is successful, it will simplify the expression to a single
double value and print it to stdout. If that is not possible **calcƶ**
will print errors to stdout. Then **calcƶ** will ask for another expression.

Currently **calcƶ** can handle:
* Spaces (they will be ignored)
* Numeric values (floating point values work)
* +, -, \*, / and % operators
* Braces (nesting should work)

The special keyword *exit* will terminate **calcƶ**.

A sample session in **calcƶ**:

```
sbt run
...
calcƶ> (152 - 148) * (2 + 2)
    ✓> 16.0
calcƶ> (2e - 1) * (2s * 1d)
    ✗> Cannot parse '2s'
calcƶ> exit
``` 

