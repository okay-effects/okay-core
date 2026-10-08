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
shift0 and `$`, the answers modified), hidden — `private[zero]`, so no raw delimiter can catch a request on its
way to a handler. On it one protocol of delimiters (`Eff.scala`): a body under a delimiter, its answers read —
a value, an operation's request, a `shift`'s — and two modules written on it, each a delimiter:

| File | What is in it |
|---|---|
| `Cont0.scala` | the kernel, hidden |
| `Rows.scala` | `Row`, `+:`, `Empty`, `In`, `%`, `Sub`, `Req`, `Paths` |
| `Eff.scala` | `Eff[R, J, I, O, A]` and `Level` (opaque), `Ctx` (`c.R`, `c.J`, `c.T`, `c.P[A]`, `c.Body`, `c.Reset`), `pure`, `flatMap`, `map`, `delay`, `run`; the protocol |
| `Control.scala` | `reset[S]`, `shift[A, O]` (Danvy–Filinski's: up to any delimiter — a reset, a handler, the top), `shift0[A, O]` (the body outside the reset) |
| `Effects.scala` | `Handler`, `Answering`, `perform`, `performAt`, `handle`, `Needs`, `A ! Es` |
| `std/` | `Reader`, `State`, `Writer`, `Throws`, `Choose`, `Emit` (`collect`, `generate`), `PState`, `Async` — `okay.std`'s |

A reset answers no operation — it passes every one on — and a `shift` is a request every delimiter takes alike,
its body under that delimiter again: so effects and control compose, as on `Cont`. Handlers are dynamic: the
handler is the delimiter an operation reaches where the program runs. Master's tests run on it, ported
(`src/test/scala/okay/zero/master`), their answers the same.

Not as on `Cont`: a context carries its level, `c.J`, so an answer holding a program of its own level is a
recursive type — `generate` is over the consumer's own, nominal, generator type; at the top, where a reset's
body would fix the answer before it says it, `run[A]` names it.
