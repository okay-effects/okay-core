## async-module - Async, a module of its own

`async/` (package `okay.async`, artifact okay-core-async): `async`, `await`, `Async.run`, `runAsync`, asynchronous
streams (`Async.stream`, `Async.Generator`), and what they race and wait with — `Swap`, `Mailbox`, `Platform`. The
core (`okay.core`, `okay.std`) has no thread, no wait, no callback in it now. The module depends on the core and on
its test helpers (backends, references); its tests (`TestAsyncOn`, `TestAsyncStreamOn`) moved with it, docs/guide.md's
Async section is docs/async.md, compiled and checked by the module's `TestAsyncDoc` (project/GuideDoc.scala takes a
package and a name now); `MonadsBenchmark.asyncAwaits` is `async/`'s `AsyncBenchmark.asyncAwaits` (8.6 µs a thousand on
Cont, as before). `sbt test` runs the core's, the module's and Scala 2's suites.
