# Guide

How to write programs with effects and delimited control, run them, and write effects of your own. The guide's own
text is compiled: `project/GuideDoc.scala` makes its `scala` blocks one object, and `TestGuideDoc` checks each line
`expr   // value` against the value written. `TestGuide` runs the examples on both monads besides.

```scala
import okay.core.*                       // the interface: Effects, Handler, perform, rows, A ! Es
import okay.core.Control.{reset, shift}  // delimited control in context
import okay.std.*                        // the standard effects
```

## Programs and their context

A program is written against a **context**, `(using c: Effects)`, and is a value of `c.Prog[A]`: a program of value
`A`. The context is the instance — it says which monad the program is built in, which effects are in force here
(`c.Effs`, a row) and what the answer at this point is (`c.S`). A program never names the monad; the same program
runs on every monad that implements the interface.

```scala
def greet(using c: Effects, r: Reader % String :? c.Effs, w: Writer % String :? c.Effs): c.Prog[Int] =
  ask[String].flatMap(name => tell(s"hello, $name").map(_ => name.length))
```

- `flatMap` and `map` are the context's own; `pure(a)` (or `c.pure(a)`) is a value as a program; `c.delay(p)` builds
  `p` when it is run — a tail call in constant stack.
- `Reader % String :? c.Effs` says: the effect `Reader` of `String` is in the row of this context. The compiler finds
  the path to it; nothing else names the row.

## Rows

```scala
type Row = Reader % Int +: Writer % String +: Pure   // a row: Reader of Int, then Writer of String
```

- `Pure` is the empty row, `E +: R` the effect `E` in front of the row `R`.
- `F % S` gives an effect of two parameters its first: `Reader % Int` is the effect `[X] =>> Reader[Int, X]`.
- `E :? R` is the path to `E` in `R` — what a program asks for.
- The row is built by the handlers, outside in; a program only names the effects it needs.

### `A ! Es`

The short form: a program of value `A` in any context whose row has every effect of `Es`, in any order.

```scala
def inc: Int ! Reader % Int = ask[Int].map(_ + 1)
def both: Int ! Reader % Int +: Writer % String +: Pure =
  ask[Int].flatMap(a => tell(a.toString).map(_ => a))
```

`Es` is one effect, or a row of them ending in `Pure`.

## Running

The same program, run on the monad of your choice:

```scala
okay.cont.run(reader("Ann")(writer[String, Int](greet)))   // (List("hello, Ann"), 3), on Cont
okay.zero.run(reader("Ann")(writer[String, Int](greet)))   // (List("hello, Ann"), 3), on Cont0
```

| | `okay.cont` — `Cont` | `okay.zero` — `Cont0` |
|---|---|---|
| control | shift and reset (Danvy–Filinski) | shift0 and `$` (Materzok–Biernacki); shift and reset on them |
| a handler | a fold over the program's tree | a delimiter run by a machine |
| an operation across other handlers | goes out through each of them | answered in place if its handler answers in place — 2–5× faster |
| `shift0` | — | yes |
| a `shift` directly under a handler | `k` the rest under the handler's `ret` | `k` the rest *with* the handler |

Both run in constant stack: a loop of 100 000 operations is fine.

```scala
def counting(i: Int)(using c: Effects, s: State % Int :? c.Effs): c.Prog[Int] =
  if i == 0 then get[Int] else get[Int].flatMap(x => put(x + 1)).flatMap(_ => counting(i - 1))

okay.zero.run(state[Int](0)(counting(100000)))   // (100000, 100000)
```

At the top `run` sometimes needs the answer named: `okay.cont.run[Int](...)`.

## The standard effects (`okay.std`)

| Effect | Operations | Handler → answer |
|---|---|---|
| `Reader[E]` | `ask[E]` | `reader(e)(body)` → `A` |
| `State[S]` | `get[S]`, `put(s)`, `modify(f)` | `state(s0)(body)` → `(S, A)` — a cell per run |
| `Writer[W]` | `tell(w)` | `writer[W, A](body)` → `(List[W], A)` |
| `Throws[E]` | `raise[E, A](e)` | `throws[E, A](body)` → `Either[E, A]` |
| `Choose` | `among(as)` — multi-shot | `choose[A](body)` → `Seq[A]`, every path |
| `Emit[W]` | `yield_(w)` | `collect(body)` → `(List[W], A)`; `source(top)(body)` → `Generator[W]`, pushed or pulled; `generate(next, done)(body)` — lazy |
| `PState` | `st.get[S]`, `st.put[S](s2)` | `pstate(s0)(st => body)` — the state's *type* may change |
| `Async` | `async(a)`, `await(register)` | `Async.run(body)` on the context's own monad; `Async.source(top)(body)` |

The order of handlers is the semantics: `state(throws(body))` keeps the state of a failed body,
`throws(state(body))` loses it.

```scala
def pairs(using c: Effects, ch: Choose :? c.Effs): c.Prog[(Int, Char)] =
  among(Seq(1, 2)).flatMap(n => among(Seq('a', 'b')).map(ch => (n, ch)))

okay.zero.run(choose[(Int, Char)](pairs))   // Seq((1,'a'), (1,'b'), (2,'a'), (2,'b'))
```

## An effect of your own

An effect is an enum of its operations, each with its value's type:

```scala
enum Log[+A]:
  case Line(s: String) extends Log[Unit]

def log(s: String)(using c: Effects, in: Log :? c.Effs): c.Prog[Unit] = perform(Log.Line(s))
```

A handler gives the body's value its answer (`ret`) and each operation its clause, with `k`, the rest of the body
up to the handler. Handlers are deep: `k` runs under the same handler again.

```scala
def logging[A](using c: Effects)(body: c.Body[Log, (List[String], A), A]): c.Prog[(List[String], A)] =
  Effects.handle(new Handler[c.Prog, Log, A, (List[String], A)]:
    def ret(a: A): (List[String], A) = (Nil, a)
    def apply[X](op: Log[X], k: X => c.Prog[(List[String], A)]): c.Prog[(List[String], A)] = op match
      case Log.Line(s) => k(()).map((lines, a) => (s :: lines, a)))(body)
```

- `c.Body[E, Ans, A]` is the handler's body: a program of value `A`, in a context with `E` in front of the row, at
  the handler's answer `Ans`.
- `k` is lazy: calling it builds the rest, running it is up to the clause. A clause may call `k` once, twice
  (multi-shot, as `choose`), or never (as `throws`).

### Answering in place

A handler whose every clause is `k(value(op))` — it only gives the operation a value — should be an `Answering`. Its
operations are then answered where they are performed, with no capture; on `Cont0` even across other handlers.

```scala
enum Clock[+A]:
  case Now extends Clock[Long]

def now(using c: Effects, in: Clock :? c.Effs): c.Prog[Long] = perform(Clock.Now)

def fixed[A](using c: Effects)(t: Long)(body: c.Body[Clock, A, A]): c.Prog[A] =
  Effects.handle(new Answering[c.Prog, Clock, A, A]:
    def ret(a: A): A = a
    def value[X](op: Clock[X]): X = op match
      case Clock.Now => t)(body)
```

```scala
def prog(using c: Effects, l: Log :? c.Effs, k: Clock :? c.Effs): c.Prog[Long] =
  now.flatMap(t => log(s"at $t").map(_ => t * 2))

okay.zero.run(logging(fixed(21)(prog)))   // (List("at 21"), 42)
```

### Handlers are dynamic

A program's operation goes to the handler it *runs* under, not the one it was built under: a generator's rest,
returned out of its `reader(7)` and run under `reader(8)`, asks 8.

## Delimited control

`reset` delimits, `shift` captures the rest up to the nearest delimiter — a `reset`, a handler, or the top — as `k`:

```scala
def twice(using c: Effects): c.Prog[Int] =
  reset[Int](shift[Int, Int](k => k(10).flatMap(k)).map(_ + 1))

def modified(using c: Effects): c.Prog[String] =
  reset[Int](shift[Int, String](k => k(5).map(_.toString)).map(_ * 2))

okay.zero.run(twice)      // 12
okay.zero.run(modified)   // "10"
```

- `shift[A, O]`: `A` the value at the hole, `O` the answer the delimiter is moved to. The answer type may change
  (Asai–Kameyama): `modified` resets at `Int` and answers a `String`.
- `k` is the rest *with* the delimiter (Danvy–Filinski): `k(x)` runs the rest under a delimiter of its own; the
  shift's body runs under the delimiter again.
- Effects and control compose: a `reset` passes every operation on; a shift's body and `k` may perform.

### A `shift` directly under a handler

The one place where the monads differ by design. With no `reset` between, the delimiter is the handler:

- on `Cont0`, `k` is the rest with the handler, its clauses too: a `raise` in `k` is that `k`'s `Left`, and the
  shift's body goes on;
- on `Cont`, `k` is the rest under the handler's `ret` only, its operations the shift body's handler's: a `raise`
  in `k` aborts the shift's body.

With a `reset` between, both agree.

### `shift0` (`Cont0` only)

`shift0` captures up to the nearest `reset` like `shift`, but its body runs *outside* the reset, in its place — so a
`shift0` in it reaches the next reset out.

```scala
import okay.zero.shift0
okay.zero.run[Int](reset[Int](shift0[Int, Int](k => k(1).map(_ + 1)).map(_ * 10)))   // 11
```

It needs a reset's context of `Cont0`: under a handler, or on `Cont`, it does not compile.

## State whose type changes: `PState`

`pstate` threads a state through the answer type: `put` of another type changes the state's type for the rest, and
a `get` of the wrong type does not compile.

```scala
okay.zero.run[(String, Int)](pstate(7)(st => st.get[Int].flatMap(n => st.put[Int]("x" * n)).flatMap(_ => st.get[String]).map(_.length)))   // ("xxxxxxx", 7)
```

## Generators

`source(top)(body)` makes the body a `Generator[W]`: a source of its elements, not started, on the monad of the top
given — `okay.zero.top`, `okay.cont.top`, or a context's own `c.top`. A value: each consumption runs it from the
beginning.

```scala
def fibs(a: Long, b: Long)(using c: Effects, e: Emit % Long :? c.Effs): c.Prog[Unit] =
  yield_(a).flatMap(_ => fibs(b, a + b))

source[Long](okay.zero.top)(fibs(0, 1)).take(8)   // List(0, 1, 1, 2, 3, 5, 8, 13)
source[Long](okay.cont.top)(fibs(0, 1)).take(8)   // the same, on Cont
```

Every `Source[W]` is consumed the same way:

- **pushed** — `foreach`, `fold`, `toList`: the body run to its end, each element handed on where it is yielded.
  For a source that ends; the fastest pass.
- **pulled** — `take(n)`, `iterator`: as far as asked; the rest never runs. `iterator` is closeable: `close()`
  ends the run where it is.
- `map`, `filter`, `flatMap`, for-comprehensions; `Source.of(ws*)`.

```scala
(for n <- source[Long](okay.zero.top)(fibs(0, 1)) if n % 2 == 0; s <- Source.of(n, -n) yield s).take(6)   // List(0, 0, 2, -2, 8, -8)
```

A generator's `map` and `filter` are fused: composed into what its handler does with an element, no object an
element of their own. `start` runs a generator to its first element, a `Stream[W]` — one type whatever the monad:
an enum, `Next(w, top, rest)` (the rest a program of `top`'s monad, its type `top`'s) or `Done()`; `pull()` runs the
rest, as often as asked. A `Stream` made from another (`map`, `flatMap`, `++`, `Stream.of`) has a third case,
`Later(w, rest)`, its rest a function: the rest of a started generator is a program already made, its answer
fixed, so mapping it wraps each element; mapping a `Source` changes what the handler makes.

Effects the body handles itself are in the source: `source(top)(reader(5)(body))`. A `state` there is one cell a
run. Under effects still to be handled, `generate(next, done)(body)`: a generator of the consumer's own type.

## Async

`async(a)` suspends a computation, `await(register)` waits on a callback. `Async.run(body)` runs the body on the
context's own monad, at its top (`c.top`) — the run goes on in the callback's thread when the callback comes later; its failure is
raised in the context as `Throws % Throwable`.

```scala
def io(using c: Effects, a: Async :? c.Effs): c.Prog[Int] = async(20).flatMap(x => async(x + 1))
okay.zero.run(throws[Throwable, Int](Async.run(io)))   // Right(21)
```

### Asynchronous sources

A body may yield and wait: `Emit` and `Async` together. `Async.source(top)(body)` is a `Source` as above, its
`map` and `filter` fused. Pulled, a `yield_` waits until the next element is asked for — the body never runs
ahead of its consumer — and `take` or `close()` cancels the rest: the run aborted where it waits. Pushed,
`subscribe(f)(done)` runs it without waiting, `f` of each element in the thread the run is in; `foreach`, `fold`,
`toList` wait for the end. A failure is thrown by the consumer.

```scala
def ticks(using c: Effects, e: Emit % Int :? c.Effs, a: Async :? c.Effs): c.Prog[Unit] =
  async(1).flatMap(yield_(_)).flatMap(_ => async(2).flatMap(yield_(_)))

Async.source[Int](okay.zero.top)(ticks).toList   // List(1, 2)
```

## Notes

- An effect the context does not have does not compile: there is no path to it.
- A program is a value: built once, run many times; `state` makes a fresh cell each run.
- Not every program is stack safe on every monad: `Cont` and `Cont0` are; the textbook reference in the tests is not.
