# okay-core

Algebraic effects and handlers for Scala 3 — on the JVM, Scala.js and Scala Native, and from Scala 2.13 — with
delimited control, on **one monad**: `Cont[R, I, O, A]`, effects with rows in the monad of shift0 and `$`
(`okay.one`), a CEK machine with `k` as data; and over it the facade, `A ! E1 + E2`.

No macros, no runtime tests of an operation's class, no cast: an operation carries a path to its effect in the row,
and a handler follows it; a handler's `k` is a program over the row outside, resumed any number of times, anywhere
that row is handled. Effects can be put under CONTROL AND AUDIT: an effect marked sensitive is handled only under
its audit — the types refuse anything else — and an observer sees every operation of every effect and its answer
([docs/intercept.md](docs/intercept.md)). (okay-kernel's programs, run natively by the same machine, are untyped by the kernel's design:
their operations are found by identity — `okay.core.kernel`.)

## A taste

```scala
import okay.*

def greet: Int ! Reader % String + Writer % String =                  // a program as its type says it
  ask[String].flatMap(name => tell(s"hello, $name").map(_ => name.length))

def twice[T <: Row]: Eff[T, Int] =                                      // control, an effect of the row
  reset[Int, T](shift[Int, T, Int](k => k(10).flatMap(k)).at[Shifts[Int, T] +: T].map(_ + 1))

run(reader("Ann")(writer[String, Int, Reader % String +: Pure](greet)))   // (List("hello, Ann"), 3)
run(twice[Pure])                                                         // 12
```

`A ! E1 + E2` is all a program's type says: a program of value `A`, in any place that has those effects — in any
order, among any others, under any handlers. `!`, `+` and `%` are all a user needs to know; rows, paths, places and
the machine are the library's own (`okay.core`, `okay.one`). These examples are compiled and run by `TestGuide`.
`import okay.*` is the artifact `okay-core-words` (`words/`, one package object over `okay.core` and `okay.core.std`): the
core itself puts nothing at the top of the package `okay`, so the okay platform — whose own `okay.*` are its words
over `Effects[M]` — depends on the core with no clash on one classpath; a user of okay-core alone adds the words.

## Documentation

- [Guide](docs/guide.md) — programs, `A ! E1 + E2`, rows, the standard effects, your own effects and handlers,
  answering in place, control's two faces, streams, intercepting (`local`, `handleWith`), control and audit
  (`audit`, `observe`), `Eff[R, A]` and okay's interface.
- [Control and audit of effects](docs/intercept.md) — interceptors: a sensitive effect handled only under its audit,
  guaranteed by the types; each operation told to a trail, checked by a policy, changed or refused; an observer of
  every effect.
- [okay-core on one monad: the landing plan](docs/one.md) — the decision of 2026-10-13: one monad, `k` as data, what
  stays and what goes, the kernel's acceptance, the gates in order — and where the landing stands.
- [Architecture](docs/architecture.md) — the kernel, the machine, the facade, tests, benchmarks (being re-written for
  the one monad; the kernel's own account is okay-one's README).
- [okay-core as an implementation of okay-kernel](docs/kernel.md) — native: how the machine runs the kernel's
  programs, what is exact.
- The history of the design in [`changelog.d`](changelog.d), the benchmark runs in [`bench.d`](bench.d).

## Layout

| Package | What is in it |
|---|---|
| `okay.one` | THE KERNEL (it lives here; written first in the okay-one repository, its last import 45189cb): `Cont[R, I, O, A]`, the rows (`row`: `Row`, `+:`, `Pure`, `In`, `Removed`), the handler forms (`handler`: `Answering`, `Stateful`, `Translating`, `Aborting`, `Deep`), the machine of one `Meta` (`machine`), `Step`, its std (`Reader`, `State`, `Writer`, `Throws`, `Choose`, `Shift % S`, `Async`) and its suites (`TestReference`, the meaning in CPS) |
| `okay.core` | THE FACADE over it: `A ! E1 + E2` (`!`, `+`, `%`: all a program's type needs) — a program fitted to the place it runs at (`Needs`, `Place`, `Path`); `Eff[R, A]`, the kernel's program over a row named, `E :? R`, `Has[E]`; `Op`, an operation bound where it is used; `Handler`, `Answering`; `handle`, `handleIn`, `translate`, `intercept`, `run`; `Control`: `reset`/`shift`/`shift0` over `Shifts[S, T]`, `cont`/`lower` for `Control[R]`; `Atm[A, X, Y, Es]`, the answer moved with no row named — `Atm.shift0`, `Atm.dollar`, `handleAt` for the handlers inside the `$`; `RowEffects`, okay's `Effects[M]` over `Eff` ([docs/okay.md](docs/okay.md)) |
| `okay.core.std` | the effects by their names, the kernel's std re-exported (one std): `reader` (`local`), `state` (each path its own), `cell`, `writer`, `throws` (`handleWith`), `choose`, `first`, `Emit` (`collect`, `generate`, `stream`, `Stream`, `Step`), `pstate` (the kernel's `PState`, lowered) |
| `okay.*` | THE WORDS, `import okay.*` — `okay-core-words` (`words/`), an artifact of its own over `okay.core` and `okay.core.std` |
| `okay.core.kernel` | `OnCore`: okay-kernel's `Effects` and `Control` on the one monad — every delimiter of the bridge a `Deep` handler of one request effect, export through `Step` ([docs/kernel.md](docs/kernel.md)); okay-kernel's TCK in the module `kernel/` |
| `okay.tx` (module `tx/`) | TRANSACTIONS AS A SUBTREE: `Tx` — `Atomic(body)` a bracket holding its body, `Rollback` a leaf; the handler the resource's owner (`Transactor`: begin, commit, rollback, retry) walking the tree with one bit, closed or open — one begin, one commit however deep; `atomic`, `rollback`, `transactions`; on the JVM `Jdbc`, a transactor over a JDBC connection |
| `okay.async` (module `async/`) | `async`, `await`, `Async.run`, asynchronous streams — the program stepped (`Step`), the driver outside; `Ref`, `Mailbox`: the core has no thread in it |
| `okay.interop` (modules `interop/cats`, `interop/tofu`) | cats' monad over `Eff[R, *]` (every platform); tofu's `WithLocal` and `Errors` from an effect anywhere in the row, by `Intercept.at` (the JVM's) |
| `okay.scala2` (modules `scala2/*`) | for Scala 2.13: short names of effects and rows, the same cats and tofu instances compiled by scalac 2.13; `okay.core.Ops` — the kernel's words Scala 2 can read (no inline, no extension): `perform`, `handle`, `run`, the paths as implicits |

## The one monad

`Cont[R, I, O, A]`: a program of `A` over `R`; `R` a row of effects — then `I = O = Nothing`, a program of effects
carries no answer types, which is what lets it run under any handlers — or `Control[R]`, delimited control with the
answer type modified (Materzok–Biernacki's shift0 and `$`, typed as Asai–Kameyama's), its effects answered in
place. One machine, `eval(c, k, m)`: the node, the frames of its level, the levels outside — a handler each, or a
`$`; an operation answered in place goes straight into the frame on top; else out past each level, each put around
its rest in a `Cont` — `k` as data, put back on the `Meta` in force where it is resumed. Everything in constant
stack: operations, binds, handlers nested 100 000 deep, resets. The specification: `TestReference`, every node's
meaning in CPS, random programs' answers pinned by a digest.

`MonadsBenchmark`'s lanes on it (bench.d/2026-10-14T07-08-11_9557506+.csv, master 9557506, JDK 26, JMH `-f 2 -wi 3 -i 5`),
beside okay-core-zero's last (bench.d/2026-10-10T14-20-56, the base of docs/one.md §4 H), µs for n = 1000:

| | okay-core-zero | the one monad |
|---|---|---|
| pure binds / tail calls | 3.21 / 2.20 | 3.13 / 2.10 |
| answered in place / by a general clause | 5.52 / 8.20 | 5.34 / 9.31 |
| an operation across 1 / 4 / 16 handlers | 6.94 / 5.54 / 5.83 | 5.43 / 5.47 / 5.77 |
| state (each path its own) / a cell / under two more handlers | 12.64 (a cell) / — / — | 11.12 / 11.05 / 11.03 |
| `Int ! State % Int` / `+ Reader % Int`, each path through the place | 13.15 / 17.88 | 12.38 / 18.91 |
| reads / under `local` | 4.62 / 14.92 | 4.65 / 13.24 |
| writer, throwing, collect | 17.42, 16.62, 12.66 | 9.28, 15.05, 11.66 |
| choose / under one more handler; 8 queens (a run) | 24.35 / 34.88; 148 | 13.09 / 18.83; **204** |
| a generator: pulled / mapped / taken / pushed | 14.88 / 16.59 / 13.81 / 7.39 | **20.61** / **24.10** / 17.95 / 7.89 |
| a shift under a reset, 16 maps, `k` 8 times (a run) / 256 maps / once | 0.31 / 4.74 / 0.04 | **0.89** / 7.19 / 0.11 |
| `MixedBenchmark`, 1000 orders: the total a state / a cell | 130 (a cell) | **223** / 223 (159 on okay-one c7af924: docs/one.md §8) |
| `KernelMixedBenchmark`, the protocol's program | 231 (zero, native) | **440** (a library over `Deep`, docs/kernel.md) |

The one monad is ahead on the lanes `k` works hardest on — choose, writer, `local`, state — and at the bar (§4 H)
on most, an operation across 16 handlers too (the look-up kept on the path); behind it, each the kernel's (the lanes
are written in its own forms): a generator pulled (`k` as data put back a level at a time), control as an effect
against a native capture, 8 queens, the mixed program — 159 on okay-one c7af924, 221 since cec1b6a, the account in
docs/one.md §8 — and the kernel bridge (`OnCore`, a library over `Deep`).

## Tests

`sbt coreJVM/test`: okay-one's suites as imported (66), the facade's (TestEffects, TestContext, TestControl,
TestDynamic, TestIntercept, TestNesting, TestTogether, TestStream, TestAnywhere, TestFacade), the guide's text
compiled and checked (`TestGuide`, `TestGuideDoc`), the kernel bridge's (`TestNative`) — 208; `sbt kernel/test`:
okay-kernel's TCK on `OnCore` and the bridge's own — 30; `asyncJVM/test` 19, `interopCatsJVM/test` 2,
`interopTofu/test` 9 (docs/interop.md compiled), `scala2/test` 6 (from Scala 2.13). Scala Native: the core, async
and cats compile and test; Scala.js: they compile, the test bridge does not link (on master too — the toolchain's).

## Build

```sh
sbt coreJVM/test kernel/test                  # the core and the kernel bridge
sbt bench                                     # JMH, then a summary table (project/Bench.scala)
sbt "bench -f 1 MonadsBenchmark.state"
sbt "bench save -prof gc"                     # keep the run in bench.d/ (index.tsv + CSV)
sbt "benchHistory <regexp>"                   # the benchmarks matching it, run by run
```

Scala 3.9.0 (and 2.13.18 for `scala2/`), Scala.js 1.22.0, Scala Native 0.5.12, sbt 1.13.0, JDK 25 or later; tests
only: munit 1.2.3, tofu 0.14.3, cats 2.13.0. The benchmarks run on the JDK sbt runs on, JDK 26 for the runs in
`bench.d` so far; `bench` prints it and `bench save` keeps it in the index's `jvm` column.
