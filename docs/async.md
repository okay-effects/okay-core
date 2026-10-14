# Async — the module `async/`

`okay.async`, a module of its own: the core (`okay.core`, `okay.core.std`) has no thread, no wait, no callback in it.
Every example here is compiled and run by the module's `TestAsyncDoc`.

```scala
import okay.core.*
import okay.core.std.*
import okay.async.*
```

## Async, await, run

`async(a)` suspends a computation, `await(register)` waits on a callback. `Async.run(body)` runs the body, this
thread waiting for it — the run goes on in the callback's thread when the callback comes later; its failure is
raised, a program needing `Throws % Throwable`.

```scala
def io: Int ! Async = async(20).flatMap(x => async(x + 1))
run(throws[Throwable, Int, Pure](Async.run(io)))   // Right(21)
```

### Asynchronous streams

A source may yield and wait — `Async.Source[W]`, `Emit` and `Async` together. `Async.stream(source)` is a `Stream` as above, its
`map` and `filter` fused. Pulled, a `yield_` waits until the next element is asked for — the body never runs
ahead of its consumer — and `take` or `close()` cancels the rest: the run aborted where it waits. Pushed,
`subscribe(f)(done)` runs it without waiting, `f` of each element in the thread the run is in; `foreach`, `fold`,
`toList` wait for the end. A failure is thrown by the consumer.

```scala
def ticks: Unit ! Emit % Int + Async = async(1).flatMap(yield_(_)).flatMap(_ => async(2).flatMap(yield_(_)))

Async.stream[Int](ticks).toList   // List(1, 2)
```


## What it is made of

- `Async`, the effect: `Run` (a computation suspended) and `Await` (a callback's value). Not handled in the program:
  the driver's, the program STEPPED (the kernel's `Step`) — `runAsync(body)(done)`: its failures handled first, each
  operation of `Async` given back at the top with the rest as data, the run going on in the callback's thread when
  the callback comes later, in place when it came already — a loop, not a frame.
- `Ref`, a cell `compareAndSet` decides on: the run and the callback race on it, which came first.
- `Mailbox`, values put and taken in order, `take` waiting: where blocking code waits for the run (`Async.run`, a
  pushed or pulled async stream). `Platform.waits`: on JS nothing can be waited for, so `take` of an empty mailbox
  fails — the run did not complete at once; the non-blocking roads (`runAsync`, `subscribe`) work everywhere.
