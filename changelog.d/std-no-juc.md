## std-no-juc - okay.std without java.util.concurrent

`Async` raced its slot with an `AtomicReference`, waited with a `CompletableFuture`; the async stream's iterator
queued with a `LinkedBlockingQueue`. Now two small things of `java.lang` every platform has: `Swap` (a cell,
`compareAndSet` under `synchronized`) and `Mailbox` (values put and taken in order, `take` waiting by
`wait`/`notifyAll`), and `Platform.waits` — where a platform cannot wait (JS, the cross build's next item), `take` of
an empty mailbox fails: the run did not complete at once. No `java.util.concurrent` left in src/main.

Measured (`MonadsBenchmark.asyncAwaits`, new: 1 000 awaits answered at once under `Async.run`, µs, Cont / Cont0):
27.1 / 26.9 → 7.8 / 8.4, 201 / 153 → 153 / 105 B — an `AtomicReference` made each await, the uncontended
`synchronized` cheaper.
