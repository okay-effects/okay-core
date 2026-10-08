# okay-core

Algebraic effects and handlers for Scala 3, on one monad of delimited
continuations with answer-type modification (Danvy–Filinski shift/reset,
Asai–Kameyama answer types). No macros, no casts, no runtime class tests:
an operation carries a path to its effect in the row, and a handler follows it.

Final tagless: programs and the effects in `okay.std` are written against two
type classes over an indexed monad `M[R, I, O, A]` — `Effects` (operations and
handlers) and `Control` (shift and reset) — and the context a program is
written in *is* the instance: `(using c: Effects)` gives `c.M`, the row `c.R`
and the answer `c.S`. `Cont` is the instance that runs them; the tests run the
same `Control` programs on a second one, plain CPS functions, too.

## Layout

### `okay.core` — the core

| File | What is in it |
|---|---|
| `Cont.scala` | `Row`, `+:`, `Pure`, `In` (a path in a row) and `E :? R` (the same in a signature), `Cont[R, I, O, A]` (the monad: `Return`, `Inject`, `Suspend`, `Bind`, `Shift`, `Map`, `Delay`, `Reset`, `Push`), `Frames` (the loop's stack), `Free[R, A]` |
| `Effects.scala` | `IxMonad` (`M`, `pure`, `delay`, `flatMap`, `map`), `Context` (`R`, `S`), `Effects` (`perform`, `handle`, `under`), `Effects.At` (the instance over `Cont`), `run`, `handle`, `Handler`, `Answering` (tail-resumptive handlers, answered in place), `perform`, `%` and `A ! Es` |
| `Control.scala` | `Control` (`shift`, `reset`), and `Control.shift`, `Control.reset` in context |

### `okay.std` — the effects, written on the core

| Effect | Operations | Handler |
|---|---|---|
| `Reader` | `ask` | `reader(e)(body)` |
| `State` | `get`, `put`, `modify` | `state(s0)(body)` → `(S, A)` |
| `Writer` | `tell` | `writer[W, A](body)` → `(List[W], A)` |
| `Throws` | `raise` | `throws[E, A](body)` → `Either[E, A]` |
| `Choose` | `among` (multi-shot) | `choose[A](body)` → `Seq[A]` |
| `Emit` | `yield_` | `collect(body)`, `generate(body)` (lazy) |
| `PState` | `st.get`, `st.put` | `pstate(s0)(st => body)` |
| `Async` | `async`, `await` (callbacks) | `Async.runAsync(body)(done)`, `Async.run(body)` |

`PState` is a state whose *type* may change, carried by the answer type, not by
the row. `Async` fails by `Throws % Throwable`.

## Writing programs

The row is built by the handlers, outside in; a program names only the
effects it needs. Two ways to write one:

```scala
// the context as givens
def counting(i: Int)(using c: Effects, s: State % Int :? c.R): c.M[c.R, c.S, c.S, Int] =
  if i == 0 then get[Int] else get[Int].flatMap(x => put(x + 1)).flatMap(_ => counting(i - 1))

// A ! Es: a program of value A, in any context whose row has every effect of Es
def inc: Int ! Reader % Int = ask[Int].map(_ + 1)
def both: Int ! Reader % Int +: Writer % String +: Pure = ask[Int].flatMap(a => tell(a.toString).map(_ => a))

run(state[Int](0)(counting(10)))              // (10, 10)
run(reader(5)(inc))                           // 6
run(writer[String, Int](reader(5)(both)))     // (List("5"), 5)
```

`Reader % Int` is the effect `[X] =>> Reader[Int, X]`. In `A ! Es`, `Es` is one
effect or a row of them ending in `Pure`; its order is the program's own, the
context's row may be any.

`shift` and `reset` in context (`import Control.*`), any monad with `Control`:

```scala
run(reset[Int](shift[Int, Int](k => k(10).flatMap(k)).map(_ + 1)))         // 12
run(reset[Int](shift[Int, String](k => k(5).map(_.toString)).map(_ * 2)))  // "10"
```

`Async` at the top, its failure raised in the context:

```scala
run(throws[Throwable, Int](Async.run(async(20).flatMap(a => async(a + 1)))))  // Right(21)
```

## Build

```sh
sbt test                                # the tests (munit)
sbt bench                               # JMH, then a summary table (project/Bench.scala)
sbt "bench -f 1 -p n=100000 tailcall"
sbt "bench save -prof gc"               # keep the run in bench.d/ (index.tsv + CSV)
sbt "benchHistory <regexp>"             # the benchmarks matching it, run by run
```

Scala 3.9.0, sbt 1.13.0, JDK 25.

## `okay.zero` — the same on shift0

Everything above, on a kernel of four nodes: `Cont0` (`Return`, `Bind`, `Shift0`, `Dollar` — Materzok–Biernacki's
shift0 and `$`, the answers modified), a monad of its own and open. Effects and control are written over it with
the levels in the types — under a delimiter the answers are programs of the level outside — and sealed: `Eff` and
`Level` are opaque, no raw `shift0` or `$` reaches a request, no request leaves its handler.

Each node Effects and Control build is a shift0 program — an operation `shift0(k => Req(op, k))`, a handler a `$`
whose answers it reads — and the reading of those answers is there to run them so (`Eff.scala`). What runs them is
a machine (`Machine.scala`): each handler's and reset's body in a loop of its own; an operation answered in place by
the handler at its path, if that one answers in place (`Answering`), across any handlers and resets between — the
handlers in force known at run time; a `shift` and a `shift0` taken at their delimiter; a resumption entered with
the value going through its frames. The reading is left for a `shift` up to the very top.

| File | What is in it |
|---|---|
| `Cont0.scala` | the kernel: `Cont0`, its four nodes, `pure`, `shift0`, `dollar`, `reset0`, `value`; the package's nodes, the delimiters |
| `Rows.scala` | `Row`, `+:`, `Empty`, `In`, `%`, `Sub`, `Req`, `Paths` |
| `Eff.scala` | `Eff[R, J, I, O, A]` and `Level` (opaque), `Ctx` (`c.R`, `c.J`, `c.T`, `c.P[A]`, `c.Body`, `c.Reset`), `pure`, `flatMap`, `map`, `delay`, `run`; the nodes; the reading |
| `Machine.scala` | the machine: the loops, the handlers in force, a resumption, a shift at its delimiter |
| `Control.scala` | `reset[S]`, `shift[A, O]`, `shift0[A, O]` (the body outside the reset), `Same` |
| `Effects.scala` | `Handler`, `Answering`, `perform`, `performAt`, `handle`, `Needs`, `A ! Es` |
| `std/` | `Reader`, `State`, `Writer`, `Throws`, `Choose`, `Emit` (`collect`, `generate`), `PState`, `Async` — `okay.std`'s |

A reset answers no operation — it passes every one on — and a `shift` is Danvy–Filinski's up to any delimiter: a
reset, a handler, the top. Its body runs under the delimiter again, `k` is the rest WITH the delimiter — under a
handler `k = λx. handle(E[x])`, the clauses too: an operation in `k(x)` is `k`'s own handler's, its clause with
the rest of `k` only (a `raise` in `k` is that `k`'s `Left`, the shift's body goes on). The clauses are at the
handler's answer, so a shift that MOVES the answer (`O` not the context's `T`) is allowed in a reset only — `Same`,
a compile error directly under a handler. Handlers are dynamic: the handler is the delimiter an operation reaches
where the program runs. Master's tests run on it, ported (`src/test/scala/okay/zero/master`), their answers the same.

Not as on `Cont`: a context carries its level, `c.J`, so an answer holding a program of its own level is a
recursive type — `generate` is over the consumer's own, nominal, generator type; at the top, where a reset's
body would fix the answer before it says it, `run[A]` names it.

Against master, the same programs (`src/jmh/scala/okay/zero`, `sbt "bench -prof gc -f 2"`, JDK 25, one run;
ns an operation, a run for the captures):

| | master | zero |
|---|---|---|
| an operation, answered in place (`tailcallHandled`) | 5.50 | 5.19 |
| an operation, the general clause (`tailcallHandledGeneral`) | 8.73 | 7.43 |
| an operation across a handler (`tailcallForwarded`) | 11.53 | 5.27 |
| across 1 / 4 / 16 handlers (`forwardedDepth`) | 9.17 / 21.50 / 78.79 | 4.53 / 5.64 / 16.63 |
| state, writer, throws (`EffectsBenchmark`) | 15.03, 19.94, 13.12 | 14.03, 17.23, 13.22 |
| choose (multi-shot) | 28.87 | 31.30 |
| a shift under a reset, depth 1 / 16 / 256, `k` 8 times | 127 / 418 / 5730 | 119 / 301 / 4510 |
| a shift under a handler, depth 1 / 16 / 256, `k` 8 times | 116 / 401 / 6320 | 134 / 322 / 4630 |
| a shift under a handler, depth 1, `k` once | 29 | 37 |
| generators: collect, generate | 11.43, 15.87 | 9.45, 13.95 |
| pure: tail calls, left binds, left maps, fib | 3.16, 5.67, 4.75, 3.75 | 3.42, 5.61, 4.35, 3.89 |

Behind master: choose (+8%), a shallow shift under a handler, pure tail calls (+4…8%). `shift0`, master has none:
27 ns a run, `k` once (`ZeroShift0Benchmark`).

## One interface, several monads

Master's `Effects` and `Control` are over any monad of levels: a delimiter's context is a member of the context
— `Handled[E, S]` under a handler, `Reset[S]` in a reset, `Answer[S]` in a shift's body — and a delimiter's body is
in `Inner`, the monad one level in (`Cont`'s the same, `M`). So a program written once, `(using c: Effects)`, and
`okay.std`, run on each instance: `Cont` (`Effects.At`), the CPS reference (`Cps.At`, control only, in the tests)
and `Cont0` (`okay.zero.Fx`). Master's tests run so on `Cont` and on `Cont0` (`TestEffectsOn`, `TestContextOn`,
`TestPStateOn` over a `Backend`); `MonadsBenchmark` runs the same programs on both, `backend=cont|zero`:

| ns an operation | cont | zero |
|---|---|---|
| pure binds | 3.18 | 3.18 |
| answered in place / the general clause | 5.43 / 8.77 | 5.42 / 7.62 |
| across 1 / 16 handlers | 11.77 / 93.99 | 5.66 / 17.34 |
| state, writer, throws, choose | 14.79, 19.87, 13.87, 28.73 | 14.34, 17.47, 15.53, 32.63 |
| collect | 11.89 | 10.31 |
| a shift under a reset, 16 maps, `k` 8 times (a run) | 430 | 317 |

On `Cont0` through the interface a handler is master's, made one of `okay.zero`'s at its install: a handler
installed and aborted (`throws`) pays for it, 15.5 against `okay.zero`'s own 13.2.
