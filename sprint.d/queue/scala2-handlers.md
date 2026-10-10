# scala2-handlers - one's own handlers from Scala 2

`Program.Handler`'s clause gets an object of plain methods (`pure`, `flatMap`, `map`) for the programs outside, not
Scala 3 extension methods Scala 2 cannot see. A test: an effect and its handler written in Scala 2, on both monads.

Done when: `scala2/` defines its own effect and handler and runs it.
