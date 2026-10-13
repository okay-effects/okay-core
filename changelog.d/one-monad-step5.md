## one-monad, step 5 - async/ over Step

- `okay.async` on the one monad: `runAsync(body)(done)` STEPS the program — its failures handled first (`Throws`
  taken from the row wherever it is, by `Removed`), each operation of `Async` given back at the top with the rest as
  data (`Step.Yield`); a computation answered at once, a callback that came while registering taken in place, a
  later one driving on from its thread — a loop, not a frame. `Async.run(body)` a program needing
  `Throws % Throwable`; `Async.stream(body)` a `Generator` on `Deep`, `k` as data; `Ref`, `Mailbox`, `Platform`
  unchanged. No `Top`, no backend: one monad.
- `TestAsync` 7, `TestAsyncStream` 5, `TestAsyncCross` 4 (one JS-only), `TestAsyncDoc` (docs/async.md compiled) —
  19 on the JVM; the module compiles on JS and Native. `AsyncBenchmark` over the one monad.
