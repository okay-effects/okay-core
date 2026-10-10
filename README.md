# okay-core

Algebraic effects and handlers for Scala 3 — on the JVM, Scala.js and Scala Native, and from Scala 2.13 — with
delimited control — `shift`/`reset` with the answer type modified
(Danvy–Filinski, Asai–Kameyama) — written **once** over an interface and run on **two monads**:

- **`Cont0`** (`okay.zero`), **the default** — Materzok–Biernacki's shift0 and `$`, the levels in the types; a
  handler a delimiter run by a machine that answers operations in place;
- **`Cont`** (`okay.cont`) — shift and reset; a handler a fold over the program. A second implementation of the same
  interface, a different one: what keeps the interface an interface, not one monad's API.

No macros, no casts, no runtime tests of an operation's class: an operation carries a path to its effect in the row,
and a handler follows it.

## A taste

```scala
import okay.*

def greet: Int ! Reader % String + Writer % String =               // a program as its type says it
  ask[String].flatMap(name => tell(s"hello, $name").map(_ => name.length))

def twice: Int ! Reader % Int =                                     // control, in the same program
  reset[Int](shift[Int, Int](k => k(10).flatMap(k)).map(_ + 1))

run(reader("Ann")(writer[String, Int](greet)))                     // (List("hello, Ann"), 3), on Cont0
runOn(okay.cont.top)(reader("Ann")(writer[String, Int](greet)))    // the same, on Cont
```

`A ! E1 + E2` is all a program's type says: a program of value `A`, in any context that has those effects — in any
order, among any others, under any handlers. `!`, `+` and `%` are all a user needs to know; contexts, rows and
paths, the monads and their machines are the library's own (`okay.core`, `okay.cont`, `okay.zero`). These examples
are compiled and run by `TestGuide`.

## Documentation

- [Guide](docs/guide.md) — programs, `A ! E1 + E2`, rows, the standard effects, your own effects and handlers, answering
  in place, `shift`/`reset`/`shift0`, `PState`, streams, intercepting (`local`, `handleWith`), `Program[R, A]`,
  tofu, Scala 2, `Async`.
- [What `Cont0` does better than `Cont`](docs/cont0-vs-cont.md) — and where it does not.
- [Architecture](docs/architecture.md) — the interface, `Cont`, `Cont0` and its machine, the references, tests,
  benchmarks.
- The history of the design in [`changelog.d`](changelog.d), the benchmark runs in [`bench.d`](bench.d).

## Layout

| Package | What is in it |
|---|---|
| `okay.core` | the interface: rows (`Row`, `+:`, `Pure`, `E :? R`, `%`), `Context`, `Control` (`shift`, `reset`), `Effects` (`perform`, `handle`, `intercept`), `Handler`, `Answering`, `A ! E1 + E2` (`!`, `+`, `%`: all a program's type needs), `Top`; `Program[R, A]`, a program over a row built where it runs; `RowEffects`, okay's `Effects[M]` over it ([docs/okay.md](docs/okay.md)) |
| `okay.std` | the effects, written once: `Reader` (`local`), `State`, `Writer`, `Throws` (`handleWith`), `Choose`, `Emit` (`collect`, `stream`, `generate`), `PState`; `Programs`, the same as `Program`s |
| `okay.cont` | `Cont`, its loop and fold; the instance `At`; `run`; `top` |
| `okay.zero` | `Cont0` (the kernel, open as a monad of its own), the machine; the instance `At`; `run`; `top`; `shift0` |
| `okay.async` (module `async/`) | `async`, `await`, `Async.run`, asynchronous streams — and what they race and wait with (`Ref`, `Mailbox`): the core has no thread in it |
| `okay.interop` (modules `interop/cats`, `interop/tofu`) | cats' monad over `Program` (every platform), tofu's `WithLocal` and `Errors` (the JVM's) |
| `okay.scala2` (modules `scala2/prelude`, `scala2/cats`, `scala2/tofu`) | for Scala 2.13: short names of effects and rows, and the same cats and tofu instances compiled by scalac 2.13 |

## The two monads

The same program gives the same answer on both, but for one case by design: a `shift` directly under a handler,
with no `reset` between — on `Cont0` its `k` holds the handler (a `raise` in `k` is that `k`'s `Left`), on `Cont`
only the handler's `ret` (a `raise` in `k` aborts the shift's body). `shift0` is `Cont0`'s alone.

`MonadsBenchmark`, the same programs on each (bench.d, 2026-10-10T14-20, JDK 26), ns an operation:

| | `Cont` | `Cont0` |
|---|---|---|
| an operation across 1 / 4 / 16 handlers | 5.6 / 7.1 / 18.3 | 6.9 / 5.5 / 5.8 |
| answered in place / by a general clause | 5.5* / 9.9 | 5.5 / 8.2 |
| state, writer, collect | 14.2, 19.7, 11.1 | 12.6, 17.4, 12.7 |
| a generator, an element pulled / pushed | 17.2 / 7.6 | 14.9 / 7.4 |
| an ask under `local` (an interceptor) / under the reader alone | 16.5 / 4.6 | 14.9 / 4.6 |
| `Int ! State % Int` / `+ Reader % Int`, each path through the context | 13.8 / 20.6 | 13.2 / 17.9 |
| `state` as a `Program`: the state first in the row / third / over okay's interface | 23.7 / 37.9 / 24.1 | 23.1 / 38.2 / 24.0 |
| 8 queens (a run) | 160 µs | 148 µs |
| a shift under a reset, 16 maps, `k` 8 times (a run) | 493 ns | 308 ns |
| choose, throws | 26.0, 17.7 | 24.4, 16.6 |
| pure binds | 3.20 | 3.21 |
| `MixedBenchmark`, an order: a reader, a choice, a throw caught, a state, a log | 151 | 130 |

\* the full run's 7.2 ± 5.2 was noise; 5.50 ± 0.01 alone (changelog.d/cont-loop-small.md).

Handlers nested by recursion — a function installing one and calling itself inside it — run in constant stack on
both, a hundred thousand deep: each monad's loop keeps the delimiters it is inside as a stack of its own
(docs/stack-safety.md).

`Cont0` is ahead where handlers are layered (3.2× across 16), on a general clause, captures, `queens`, `choose` and
the mixed program; elsewhere the two are within some per cent — both find a handler further out that answers in
place where the operation is.

## Tests

The suites are written once and run on four monads: `Cont`, `Cont0`, and two references in the tests — `Spec`
(`Cont0` without its machine, each delimiter its definition) and `Cps0` (shift0 and `$` in textbook CPS, no code of
either monad). They agree on every suite; where `Cont0` differs by design, `TestMonads` says so.

## With tofu, and from Scala 2

`Program.Of[R]` is an `F[_]`: with cats' monad and tofu's `WithLocal` and `Errors` over it (the modules
`okay-core-cats`, `okay-core-tofu`; for Scala 2, `okay-core-scala2-cats`, `-tofu`), code written over `F[_]` with tofu
runs on okay's effects, on either monad — `local` and
`handleWith` by `intercept`, an effect handled again where it is. Scala 2.13 reads the same artifact through its
TASTy reader; `scala2/tests` holds the suites that prove it — one's own effects and handlers, tofu's 2.13 artifacts.
The core itself names no library.

## Build

```sh
sbt test                                      # every suite: the core and async/ on the JVM, JS and Native, scala2/
sbt coreJVM/test                              # one platform's: coreJVM, coreJS, coreNative (asyncJVM, …)
sbt bench                                     # JMH, then a summary table (project/Bench.scala)
sbt "bench -f 1 -p backend=zero MonadsBenchmark.state"
sbt "bench save -prof gc"                     # keep the run in bench.d/ (index.tsv + CSV)
sbt "benchHistory <regexp>"                   # the benchmarks matching it, run by run
sbt "asyncJVM/Jmh/run -prof gc"               # the async module's own benchmark
```

Scala 3.9.0 (and 2.13.18 for `scala2/`), Scala.js 1.22.0, Scala Native 0.5.12, sbt 1.13.0, JDK 25 or later; tests only: munit 1.2.3, tofu 0.14.3, cats 2.13.0. The benchmarks run on the JDK sbt runs on, JDK 26 for the runs in
`bench.d` so far; `bench` prints it and `bench save` keeps it in the index's `jvm` column.
