# okay-core

Algebraic effects and handlers for Scala 3 — and Scala 2.13, from the same artifact — with delimited control — `shift`/`reset` with the answer type modified
(Danvy–Filinski, Asai–Kameyama) — written **once** over an interface and run on **two monads**:

- **`Cont0`** (`okay.zero`), **the default** — Materzok–Biernacki's shift0 and `$`, the levels in the types; a
  handler a delimiter run by a machine that answers operations in place;
- **`Cont`** (`okay.cont`) — shift and reset; a handler a fold over the program. A second implementation of the same
  interface, a different one: what keeps the interface an interface, not one monad's API.

No macros, no casts, no runtime tests of an operation's class: an operation carries a path to its effect in the row,
and a handler follows it.

## A taste

```scala
import okay.core.*
import okay.core.Control.{reset, shift}
import okay.std.*

def greet(using c: Effects, r: Reader % String :? c.Effs, w: Writer % String :? c.Effs): c.Prog[Int] =
  ask[String].flatMap(name => tell(s"hello, $name").map(_ => name.length))

def inc: Int ! Reader % Int = ask[Int].map(_ + 1)                 // the same, written A ! Es

def twice(using c: Effects): c.Prog[Int] =                        // control, in the same context
  reset[Int](shift[Int, Int](k => k(10).flatMap(k)).map(_ + 1))

okay.cont.run(reader("Ann")(writer[String, Int](greet)))   // (List("hello, Ann"), 3), on Cont
okay.zero.run(reader("Ann")(writer[String, Int](greet)))   // the same, on Cont0
okay.zero.run(twice)                                       // 12
```

A program is written against its context, `(using c: Effects)` — the context is the instance (final tagless):
`c.Prog[A]` a program here, `c.Effs` the effects in force, `c.S` the answer. The row is built by the handlers,
outside in; a program names only the effects it needs. These examples are compiled and run by `TestGuide`.

## Documentation

- [Guide](docs/guide.md) — programs, rows, `A ! Es`, the standard effects, your own effects and handlers, answering
  in place, `shift`/`reset`/`shift0`, `PState`, streams, intercepting (`local`, `handleWith`), `Program[R, A]`,
  tofu, Scala 2, `Async`.
- [What `Cont0` does better than `Cont`](docs/cont0-vs-cont.md) — and where it does not.
- [Architecture](docs/architecture.md) — the interface, `Cont`, `Cont0` and its machine, the references, tests,
  benchmarks.
- The history of the design in [`changelog.d`](changelog.d), the benchmark runs in [`bench.d`](bench.d).

## Layout

| Package | What is in it |
|---|---|
| `okay.core` | the interface: rows (`Row`, `+:`, `Pure`, `E :? R`, `%`), `Context`, `Control` (`shift`, `reset`), `Effects` (`perform`, `handle`, `intercept`), `Handler`, `Answering`, `A ! Es`, `Top`; `Program[R, A]`, a program over a row built where it runs |
| `okay.std` | the effects, written once: `Reader` (`local`), `State`, `Writer`, `Throws` (`handleWith`), `Choose`, `Emit` (`collect`, `stream`, `generate`), `PState`, `Async`; `Programs`, the same as `Program`s |
| `okay.cont` | `Cont`, its loop and fold; the instance `At`; `run`; `top` |
| `okay.zero` | `Cont0` (the kernel, open as a monad of its own), the machine; the instance `At`; `run`; `top`; `shift0` |

## The two monads

The same program gives the same answer on both, but for one case by design: a `shift` directly under a handler,
with no `reset` between — on `Cont0` its `k` holds the handler (a `raise` in `k` is that `k`'s `Left`), on `Cont`
only the handler's `ret` (a `raise` in `k` aborts the shift's body). `shift0` is `Cont0`'s alone.

`MonadsBenchmark`, the same programs on each (bench.d, 2026-10-09T11-37), ns an operation:

| | `Cont` | `Cont0` |
|---|---|---|
| an operation across 1 / 4 / 16 handlers | 11.6 / 23.4 / 91.7 | 5.5 / 5.2 / 5.5 |
| answered in place / by a general clause | 6.43 / 8.92 | 5.41 / 7.79 |
| state, writer, collect | 16.1, 20.0, 13.1 | 14.7, 17.3, 10.5 |
| a generator, an element pulled / pushed (`stream`, measured apart) | 14.9 / 10.3 | 14.5 / 7.6 |
| 8 queens (a run) | 256 µs | 190 µs |
| a shift under a reset, 16 maps, `k` 8 times (a run) | 438 ns | 324 ns |
| choose, throws | 29.2, 13.6 | 34.3, 14.3 |
| pure binds | 3.36 | 3.42 |

`Cont0` is 2–16× faster where handlers are layered and faster in most of the rest; `Cont` is ahead by 3–17% in
`choose`, `throws` and a shallow shift directly under a handler.

## Tests

The suites are written once and run on four monads: `Cont`, `Cont0`, and two references in the tests — `Ref`
(`Cont0` without its machine, each delimiter its definition) and `Cps0` (shift0 and `$` in textbook CPS, no code of
either monad). They agree on every suite; where `Cont0` differs by design, `TestMonads` says so.

## With tofu, and from Scala 2

`Program.Of[R]` is an `F[_]`: given tofu's `Monad`, `WithLocal` and `Errors` over it (in the tests, the shape of an
optional module), code written over `F[_]` with tofu runs on okay's effects, on either monad — `local` and
`handleWith` by `intercept`, an effect handled again where it is. Scala 2.13 reads the same artifact through its
TASTy reader; `scala2/` holds the suites that prove it, tofu's 2.13 artifacts included. The core itself names no
library.

## Build

```sh
sbt test                                      # the tests (munit), Scala 3 and the Scala 2.13 ones in scala2/
sbt bench                                     # JMH, then a summary table (project/Bench.scala)
sbt "bench -f 1 -p backend=zero MonadsBenchmark.state"
sbt "bench save -prof gc"                     # keep the run in bench.d/ (index.tsv + CSV)
sbt "benchHistory <regexp>"                   # the benchmarks matching it, run by run
```

Scala 3.9.0 (and 2.13.18 for `scala2/`), sbt 1.13.0, JDK 25 or later; tests only: munit 1.2.3, tofu 0.14.3, cats 2.13.0. The benchmarks run on the JDK sbt runs on, JDK 26 for the runs in
`bench.d` so far; `bench` prints it and `bench save` keeps it in the index's `jvm` column.
