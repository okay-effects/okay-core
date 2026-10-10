# std-no-juc - okay.std without java.util.concurrent

`Async` (its slot's CAS, `Async.run`'s wait) and the async streams (the iterator's queue, `push`'s wait) use
`java.util.concurrent`. One small platform abstraction in the core instead — a cell with an atomic swap, a way to
wait for a value — with an implementation per platform: the JVM's, a plain variable on JS (one thread), Native's.

JS cannot wait: `Async.run` and the async stream's blocking iterator there only when the run completed at once, a
clear error otherwise; the non-blocking roads (`runAsync`, `subscribe`) everywhere.

Done when: no `java.util.concurrent` under src/main of the shared core; tests green; async benchmarks no slower.
