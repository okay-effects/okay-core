## cross-js-native - the core and async/ on the JVM, Scala.js and Scala Native

`core` (in ".") and `async` cross projects (`CrossType.Pure`, sbt-crossproject 1.4.0, Scala.js 1.22.0, Scala Native
0.5.12 — okay's versions); `root` aggregates every platform's and `scala2/` (on `coreJVM`), compiles nothing itself.
A platform's own sources beside the shared: tofu's and cats' suites and `TestGuideDoc` the JVM's (tofu has no JS or
Native artifacts); `LargeStack` a thread of a 1 GB stack on the JVM and Native (Native refuses 2 GB), the caller's on
JS, where the `Cps0` suites are filtered out (not stack safe); `async/`'s `Platform` one a platform — JS's cannot
wait, `Mailbox.take` of an empty mailbox there fails saying so — and a cross suite of its own (`TestAsyncCross`:
callbacks answered at once, `subscribe` with one coming later, JS's failure); its threaded suites the JVM's. The
benchmarks the JVM's, `bench.d` the build root's still (project/Bench.scala).

Found by JS: `TestAgree`'s paths miscounted a reset since the interceptors were added to it (4b763e3) — a program
asked the wrong handler, every monad failed alike with a `MatchError`, and the test took the same failure for
agreement; on JS the failure was an undefined-behaviour error the test did not catch. The count fixed; a
`MatchError` or a `ClassCastException` now fails the test as a program made wrong.

Suites: JVM 437 + 55 (async) + 7 (Scala 2); JS 319 + 4; Native 394 + 4. Benchmarks unchanged (no main source moved).
