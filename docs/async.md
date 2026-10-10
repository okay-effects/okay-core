# Async — the module `async/`

`okay.async`, a module of its own: the core (`okay.core`, `okay.std`) has no thread, no wait, no callback in it.
Every example here is compiled and run by the module's `TestAsyncDoc`.

```scala
import okay.core.*
import okay.std.*
import okay.async.*
```

## Async, await, run

`async(a)` suspends a computation, `await(register)` waits on a callback. `Async.run(body)` runs the body on the
context's own monad, at its top (`c.top`) — the run goes on in the callback's thread when the callback comes later; its failure is
raised in the context as `Throws % Throwable`.

```scala
def io(using c: Effects, a: Async :? c.Effs): c.Prog[Int] = async(20).flatMap(x => async(x + 1))
okay.zero.run(throws[Throwable, Int](Async.run(io)))   // Right(21)
```

### Asynchronous streams

A source may yield and wait — `Async.Source[W]`, `Emit` and `Async` together. `Async.stream(top)(source)` is a `Stream` as above, its
`map` and `filter` fused. Pulled, a `yield_` waits until the next element is asked for — the body never runs
ahead of its consumer — and `take` or `close()` cancels the rest: the run aborted where it waits. Pushed,
`subscribe(f)(done)` runs it without waiting, `f` of each element in the thread the run is in; `foreach`, `fold`,
`toList` wait for the end. A failure is thrown by the consumer.

```scala
def ticks(using c: Effects, e: Emit % Int :? c.Effs, a: Async :? c.Effs): c.Prog[Unit] =
  async(1).flatMap(yield_(_)).flatMap(_ => async(2).flatMap(yield_(_)))

Async.stream[Int](okay.zero.top)(ticks).toList   // List(1, 2)
```


## What it is made of

- `Async`, the effect: `Run` (a computation suspended) and `Await` (a callback's value). Its handler is at the top
  of the context's monad (`c.top`): `runAsync(body)(done)`, the run going on in the callback's thread when the
  callback comes later, in place when it came already — a loop, not a frame.
- `Swap`, a cell `compareAndSet` decides on: the run and the callback race on it, which came first.
- `Mailbox`, values put and taken in order, `take` waiting: where blocking code waits for the run (`Async.run`, a
  pushed or pulled async stream). `Platform.waits`: on JS nothing can be waited for, so `take` of an empty mailbox
  fails — the run did not complete at once; the non-blocking roads (`runAsync`, `subscribe`) work everywhere.
