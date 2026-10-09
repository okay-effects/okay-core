# okay-core

Algebraic effects and handlers for Scala 3, with delimited control — Danvy–Filinski shift/reset, Asai–Kameyama
answer types — written ONCE over an interface and run on SEVERAL MONADS: `Cont` (shift/reset) and `Cont0`
(Materzok–Biernacki's shift0 and `$`, with a machine) — and, in the tests, two references. No
macros, no casts, no runtime class tests: an operation carries a path to its effect in the row, a handler follows it.

Final tagless: a program is written against the context, `(using c: Effects)`, which is the instance: `c.Prog[A]` a
program here, `c.Effs` the row of effects, `c.S` the answer. A handler is a delimiter that answers its effect's
operations, a reset one that answers none — so `Effects` is `Control` with handlers.

Documentation: [docs/](docs/README.md) — a [guide](docs/guide.md), [what `Cont0` does better than
`Cont`](docs/cont0-vs-cont.md), the [architecture](docs/architecture.md).

## Layout

| Package | What is in it |
|---|---|
| `okay.core` | the interface: `Row`, `+:`, `Pure`, `In` and `E :? R`, `%`, `Paths` (`Rows.scala`); `Context` (the monad `M`, `Effs`, `S`, `Inner`, `Prog`), `Control` (`shift`, `reset`), `Effects` (`perform`, `handle`, `Body`), `Handler`, `Answering`, `Needs`, `A ! Es`, `perform`, `pure`, `Top` |
| `okay.std` | the effects, written once over the interface |
| `okay.cont` | `Cont[R, I, O, A]`, its loop and fold; `At`, the instance; `run`; `top` |
| `okay.zero` | `Cont0[I, O, A]` (the kernel, open), the machine; `At`, the instance, `run`; `top`; `shift0` |

A context's delimiters have contexts of their own, members of it: `Handled[E, S]` (a handler's body), `Reset[S]` (a
reset's), `Answer[S]` (a shift's); a handler's and a reset's body is in `Inner`, the monad ONE LEVEL IN — `Cont`'s
the same, `Cont0`'s a level deeper, its answers programs of the level outside.

### `okay.std` — the effects, written on the core

| Effect | Operations | Handler |
|---|---|---|
| `Reader` | `ask` | `reader(e)(body)` |
| `State` | `get`, `put`, `modify` | `state(s0)(body)` → `(S, A)` |
| `Writer` | `tell` | `writer[W, A](body)` → `(List[W], A)` |
| `Throws` | `raise` | `throws[E, A](body)` → `Either[E, A]` |
| `Choose` | `among` (multi-shot) | `choose[A](body)` → `Seq[A]` |
| `Emit` | `yield_` | `collect(body)`, `generate(next, done)(body)` (lazy, of the consumer's own type) |
| `PState` | `st.get`, `st.put` | `pstate(s0)(st => body)` |
| `Async` | `async`, `await` (callbacks) | `Async.run(body)` on the monad of the `Top` given — `okay.cont.top`, `okay.zero.top` |

`PState` is a state whose *type* may change, carried by the answer type, not by
the row. `Async` fails by `Throws % Throwable`.

## Writing programs

The row is built by the handlers, outside in; a program names only the effects it needs:

```scala
// the context as givens
def counting(i: Int)(using c: Effects, s: State % Int :? c.Effs): c.Prog[Int] =
  if i == 0 then get[Int] else get[Int].flatMap(x => put(x + 1)).flatMap(_ => counting(i - 1))

// A ! Es: a program of value A, in any context whose row has every effect of Es
def inc: Int ! Reader % Int = ask[Int].map(_ + 1)
def both: Int ! Reader % Int +: Writer % String +: Pure = ask[Int].flatMap(a => tell(a.toString).map(_ => a))

// a handler: its body in its context, one level in
def logging[A](using c: Effects)(body: c.Body[Log, (List[String], A), A]): c.Prog[(List[String], A)] =
  Effects.handle(new Handler[c.Prog, Log, A, (List[String], A)]:
    def ret(a: A) = (Nil, a)
    def apply[X](op: Log[X], k: X => c.Prog[(List[String], A)]) = op match
      case Log.Line(s) => k(()).map((ls, a) => (s :: ls, a)))(body)

// control, in the same context
def twice(using c: Effects): c.Prog[Int] = reset[Int](shift[Int, Int](k => k(10).flatMap(k)).map(_ + 1))
```

Run on a monad of your choice — the same program, the same answer:

```scala
okay.cont.run(state[Int](0)(counting(10)))      // (10, 10), on Cont
okay.zero.run(state[Int](0)(counting(10)))   // (10, 10), on Cont0
```

A handler that only answers — each clause `k(value(op))` — is an `Answering`: its operations answered in place,
on `Cont0` across any handlers and resets between.

### Where the monads differ, by design

- `shift` directly under a general handler: on `Cont` and the reference `k` is the rest under the handler's `ret`,
  an operation in it the shift body's handler's (a `raise` in `k` aborts the body); on `Cont0`'s machine `k` is the
  rest WITH the handler — Danvy–Filinski's, the handler the delimiter (a `raise` in `k` is that `k`'s `Left`).
- `shift0` is `Cont0`'s: up to a reset, its body outside, in the reset's place; under a handler it does not compile.

### The tests and the benchmarks

Written once: `TestEffectsOn`, `TestContextOn`, `TestControlOn`, `TestTogetherOn`, `TestDynamicOn`, `TestPStateOn`
take a `Backend` (a monad's run) and run on `Cont`, on `Cont0`, on the reference and on `Cps0` — shift0 and `$` in
CPS from the textbook, functions `(A => I) => O`, no code of `Cont` nor of `Cont0`: the four agree (`Cps0`, not stack
safe, runs its tests on a thread of a large stack); `TestAsyncOn` on all four. The references are in the tests: `Ref` (`Cont0` by the kernel alone, its delimiters their shift0 programs read) and
`Cps0`. `MonadsBenchmark` runs the same programs, `backend=cont|zero`:

| ns an operation (the captures, queens: a run) | cont | zero |
|---|---|---|
| pure binds / left maps / tail calls by delay | 3.27 / 4.83 / 2.40 | 3.24 / 4.84 / 2.28 |
| answered in place / the general clause | 6.50 / 8.99 | 5.43 / 7.72 |
| across 1 / 4 / 16 handlers | 11.7 / 22.8 / 90.4 | 5.7 / 6.8 / 17.2 |
| state, `A ! Es`, writer, throws, choose | 15.5, 16.1, 20.0, 13.6, 29.1 | 14.1, 14.6, 17.3, 14.6, 33.9 |
| collect / generate / Fibonacci | 13.1 / 14.5 / 4.36 | 10.5 / 14.1 / 3.92 |
| 8 queens | 255 µs | 191 µs |
| a shift under a reset, 16 maps / 256 maps, `k` 8 times | 439 / 6050 | 323 / 4740 |
| a shift under a handler, 16 maps / 1, `k` 8 times | 411 / 122 | 366 / 139 |

The full run is kept in bench.d (`sbt "bench save -prof gc"`, 2026-10-09, 8d3b4f5).

`shift0` on `Cont0` (`ZeroShift0Benchmark`): 27 ns a run, `k` once; 1000 in a row 9.2 µs.

## Build

```sh
sbt test                                # the tests (munit)
sbt bench                               # JMH, then a summary table (project/Bench.scala)
sbt "bench -f 1 -p n=100000 tailcall"
sbt "bench save -prof gc"               # keep the run in bench.d/ (index.tsv + CSV)
sbt "benchHistory <regexp>"             # the benchmarks matching it, run by run
```

Scala 3.9.0, sbt 1.13.0, JDK 25.
