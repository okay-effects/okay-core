# Guide

How to write programs with effects and delimited control, run them, and write effects of your own. The guide's own
text is compiled: `project/GuideDoc.scala` makes its `scala` blocks one object, and `TestGuideDoc` checks each line
`expr   // value` against the value written. `TestGuide` runs the examples on both monads besides.

```scala
import okay.*    // A ! E1 + E2, the standard effects, run — and for one's own: Effects, Handler, perform
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

### `A ! E1 + E2`

What a program is, as a user writes it: of value `A`, in any context that has the effects it names — in any order,
among any others, under any handlers; the row never named.

```scala
def inc: Int ! Reader % Int = ask[Int].map(_ + 1)
def both: Int ! Reader % Int + Writer % String =
  ask[Int].flatMap(a => tell(a.toString).map(_ => a))
```

`!`, `+` and `%` are all a program's type needs: `%` gives an effect of two parameters its first, `+` joins effects.
An effect not yet known joins as well — `A ! Choose + F`, okay's way to say "and whatever else the caller has";
the compiler infers `F` at the call. One's own effect's operation is a program by `effect(op)`, or `op.perform`:

```scala
def pairOf[A, F[_]](gen: A ! Choose + F): (A, A) ! Choose + F =
  gen.flatMap(a => gen.map(b => (a, b)))
def dice: Int ! Choose + Reader % Int = ask[Int].flatMap(n => among(Seq(n, n + 1)))
run(reader(1)(choose(pairOf(dice))))   // Seq((1, 1), (1, 2), (2, 1), (2, 2))
```

okay's code on okay-core — what is the same, what is renamed: [okay.md](okay.md).

## Running

The same program, run on the monad of your choice:

```scala
runOn(okay.cont.top)(reader("Ann")(writer[String, Int](greet)))   // (List("hello, Ann"), 3), on Cont
run(reader("Ann")(writer[String, Int](greet)))   // (List("hello, Ann"), 3), on Cont0
```

| | `okay.cont` — `Cont` | `okay.zero` — `Cont0` |
|---|---|---|
| control | shift and reset (Danvy–Filinski) | shift0 and `$` (Materzok–Biernacki); shift and reset on them |
| a handler | a fold over the program's tree | a delimiter run by a machine |
| an operation across other handlers | answered in place by a walk out to its handler, longer the more handlers | answered in place, its handler found once per path — across 4 to 16 handlers 1.3–3.7× faster |
| `shift0` | — | yes |
| a `shift` directly under a handler | `k` the rest under the handler's `ret` | `k` the rest *with* the handler |

Both run in constant stack: a loop of 100 000 operations is fine.

```scala
def counting(i: Int)(using c: Effects, s: State % Int :? c.Effs): c.Prog[Int] =
  if i == 0 then get[Int] else get[Int].flatMap(x => put(x + 1)).flatMap(_ => counting(i - 1))

run(state[Int](0)(counting(100000)))   // (100000, 100000)
```

At the top `run` sometimes needs the answer named: `run[Int](...)`. `run` is on `Cont0`; `runOn(okay.cont.top)(...)` on `Cont`.

## The standard effects (`okay.std`)

| Effect | Operations | Handler → answer |
|---|---|---|
| `Reader[E]` | `ask[E]`, `local(f)(body)` | `reader(e)(body)` → `A` |
| `State[S]` | `get[S]`, `put(s)`, `modify(f)` | `state(s0)(body)` → `(S, A)` — a cell per run |
| `Writer[W]` | `tell(w)` | `writer[W, A](body)` → `(List[W], A)` |
| `Throws[E]` | `raise[E, A](e)`, `handleWith(body)(f)` | `throws[E, A](body)` → `Either[E, A]` |
| `Choose` | `among(as)` — multi-shot | `choose[A](body)` → `Seq[A]`, every path |
| `Emit[W]` | `yield_(w)` | `collect(body)` → `(List[W], A)`; `stream(top)(source)` → `Generator[W]`, pushed or pulled; `generate(next, done)(body)` — lazy |
| `PState` | `st.get[S]`, `st.put[S](s2)` | `pstate(s0)(st => body)` — the state's *type* may change |

The order of handlers is the semantics: `state(throws(body))` keeps the state of a failed body,
`throws(state(body))` loses it.

```scala
def pairs(using c: Effects, ch: Choose :? c.Effs): c.Prog[(Int, Char)] =
  among(Seq(1, 2)).flatMap(n => among(Seq('a', 'b')).map(ch => (n, ch)))

run(choose[(Int, Char)](pairs))   // Seq((1,'a'), (1,'b'), (2,'a'), (2,'b'))
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

run(logging(fixed(21)(prog)))   // (List("at 21"), 42)
```

### Handlers are dynamic

A program's operation goes to the handler it *runs* under, not the one it was built under: a generator's rest,
returned out of its `reader(7)` and run under `reader(8)`, asks 8.

## Effects from effects

The standard effects are written with the same API as one's own, so whatever is said here of them is said of any.
Four ways to build on effects there are, from the least to the most:

**Operations of existing effects, no effect of one's own** — a function asking for the paths it needs, as `modify`
is `get` and `put`:

```scala
def incr(using c: Effects, s: State % Int :? c.Effs): c.Prog[Int] = modify[Int](_ + 1)

run(state[Int](0)(incr.flatMap(_ => incr)))   // (2, 2)
```

**An effect of one's own, its handler performing another outside it.** A clause runs outside its handler, in the
handler's context `c`: an operation of any effect in `c.Effs` is one it may perform. The effect the handler needs is
in its signature; its handler, the caller's, outside. (Not an `Answering`: its `value` performs nothing.)

```scala
enum Audit[+A]:
  case Note(s: String) extends Audit[Unit]

def audit(s: String)(using c: Effects, in: Audit :? c.Effs): c.Prog[Unit] = perform(Audit.Note(s))

def auditToWriter[A](using c: Effects, w: Writer % String :? c.Effs)(body: c.Body[Audit, A, A]): c.Prog[A] =
  Effects.handle(new Handler[c.Prog, Audit, A, A]:
    def ret(a: A): A = a
    def apply[X](op: Audit[X], k: X => c.Prog[A]): c.Prog[A] = op match
      case Audit.Note(s) => tell(s"[audit] $s").flatMap(_ => k(())))(body)

def audited(using c: Effects, a: Audit :? c.Effs): c.Prog[Int] = audit("a").flatMap(_ => audit("b")).map(_ => 7)

run(writer[String, Int](auditToWriter(audited)))   // (List("[audit] a", "[audit] b"), 7)
```

**An effect of one's own, its handler installing another inside, hidden.** The caller sees `Ticket` only; the state
is the handler's own:

```scala
enum Ticket[+A]:
  case Next extends Ticket[Int]

def ticket(using c: Effects, in: Ticket :? c.Effs): c.Prog[Int] = perform(Ticket.Next)

private case class Issued(n: Int)

private def issuing[A](body: A ! Ticket)(using c: Effects, s: State % Issued :? c.Effs): c.Prog[A] =
  Effects.handle(new Handler[c.Prog, Ticket, A, A]:
    def ret(a: A): A = a
    def apply[X](op: Ticket[X], k: X => c.Prog[A]): c.Prog[A] = op match
      case Ticket.Next => modify[Issued](i => Issued(i.n + 1)).flatMap(i => k(i.n)))(body)

def tickets[A](body: A ! Ticket)(using c: Effects): c.Prog[(Int, A)] =
  state[A](Issued(0))(issuing(body)).map((i, a) => (i.n, a))

def twoTickets: Int ! Ticket = ticket.flatMap(a => ticket.map(b => a * 10 + b))

run(tickets(twoTickets))   // (2, 12)
```

- The body is an `A ! Ticket`, not a `c.Body`: a `c.Body` is a program in the context right under the handler, at
  `c`; this body runs one handler further in, under the state. An `A ! Ticket` runs in any context with `Ticket`.
- The hidden effect's type is private. A `State % Int` there would be the one the body's own `get[Int]` finds, the
  nearest; `State % Issued` no one outside can name, so the caller's state is the caller's:

```scala
def mixed(using c: Effects, s: State % Int :? c.Effs): c.Prog[(Int, Int)] =
  tickets[Int](ticket.flatMap(a => ticket.map(b => a + b))).flatMap(r => incr.map(_ => r))

run(state[(Int, Int)](100)(mixed))   // (101, (2, 3))
```

**An existing effect changed for a part of the program** — an interceptor (below, *Intercepting*), its clause
passing the operation on, changed:

```scala
def shouting[A](using c: Effects, w: Writer % String :? c.Effs)(body: c.Reset[A] ?=> c.Within[A, A, A]): c.Prog[A] =
  Effects.intercept(new Handler[c.Prog, Writer % String, A, A]:
    def ret(a: A): A = a
    def apply[X](op: Writer[String, X], k: X => c.Prog[A]): c.Prog[A] = op match
      case Writer.Tell(s) => tell(s.toUpperCase).flatMap(_ => k(())))(body)

def partly(using c: Effects, w: Writer % String :? c.Effs): c.Prog[Unit] = tell("a").flatMap(_ => shouting(tell("b")))

run(writer[String, Unit](partly))   // (List("a", "B"), ())
```

For a `Program[R, A]` (below), the second way is a `Program.HandlerAt`, its clauses performing the row outside by
`ops.perform`.

## Delimited control

`reset` delimits, `shift` captures the rest up to the nearest delimiter — a `reset`, a handler, or the top — as `k`:

```scala
def twice(using c: Effects): c.Prog[Int] =
  reset[Int](shift[Int, Int](k => k(10).flatMap(k)).map(_ + 1))

def modified(using c: Effects): c.Prog[String] =
  reset[Int](shift[Int, String](k => k(5).map(_.toString)).map(_ * 2))

run(twice)      // 12
run(modified)   // "10"
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
run[(String, Int)](pstate(7)(st => st.get[Int].flatMap(n => st.put[Int]("x" * n)).flatMap(_ => st.get[String]).map(_.length)))   // ("xxxxxxx", 7)
```

## Streams

A **source** is a program that yields: `Source[W]`, under `Emit % W`. `stream(top)(source)` makes it a
`Generator[W]` — a `Stream[W]` of its elements, not started, on the monad of the top given — `okay.zero.top`, `okay.cont.top`, or a context's own `c.top`. A value: each consumption runs it from the
beginning.

```scala
def fibs(a: Long, b: Long)(using c: Effects, e: Emit % Long :? c.Effs): c.Prog[Unit] =
  yield_(a).flatMap(_ => fibs(b, a + b))

stream[Long](okay.zero.top)(fibs(0, 1)).take(8)   // List(0, 1, 1, 2, 3, 5, 8, 13)
stream[Long](okay.cont.top)(fibs(0, 1)).take(8)   // the same, on Cont
```

Every `Stream[W]` is consumed the same way:

- **pushed** — `foreach`, `fold`, `toList`: the body run to its end, each element handed on where it is yielded.
  For a stream that ends; the fastest pass.
- **pulled** — `take(n)`, `iterator`: as far as asked; the rest never runs. `iterator` is closeable: `close()`
  ends the run where it is.
- `map`, `filter`, `flatMap`, for-comprehensions; `Stream.of(ws*)`.

```scala
(for n <- stream[Long](okay.zero.top)(fibs(0, 1)) if n % 2 == 0; s <- Stream.of(n, -n) yield s).take(6)   // List(0, 0, 2, -2, 8, -8)
```

A generator's `map` and `filter` are fused: composed into what its handler does with an element, no object an
element of their own. `start` runs a generator to its first element, a `Step[W]` — one type whatever the monad:
an enum, `Next(w, top, rest)` (the rest a program of `top`'s monad, its type `top`'s) or `Done()`; `pull()` runs the
rest, as often as asked. A `Step` made from another (`map`, `flatMap`, `++`, `Step.of`) has a third case,
`Later(w, rest)`, its rest a function: the rest of a started generator is a program already made, its answer
fixed, so mapping it wraps each element; mapping a `Stream` changes what the handler makes.

Effects the source handles itself are in the stream: `stream(top)(reader(5)(body))`. A `state` there is one cell a
run. Under effects still to be handled, `generate(next, done)(body)`: a generator of the consumer's own type.

## Intercepting: `local`, `handleWith`

An effect already in the row can be handled again for a part of the program: `Effects.intercept(h)(body)` — the
body's operations of that effect go to `h`'s clauses, `k` the rest under the interceptor again; a clause runs
outside it, so its own operation of the effect reaches the handler. The standard ones:

```scala
def scaled(using c: Effects, r: Reader % Int :? c.Effs): c.Prog[(Int, Int)] =
  ask[Int].flatMap(a => local[Int, Int](_ * 10)(ask[Int]).map(b => (a, b)))

run(reader(4)(scaled))   // (4, 40)
run(throws[String, Int](handleWith[String, Int](raise[String, Int]("bad"))(e => pure(e.length))))   // Right(3)
```

Nested ones compose, as tofu's `local`s do: the inner clause's `ask` is the outer interceptor's.

## `Program[R, A]`: the row in the type

A program written in context is built where it is written. `Program[R, A]` is one not yet built: `on(c)` builds it
in any context whose row is `R`. So its type names its row — `F[A]` for code written over `F[_]`, okay's `A ! R`,
what Scala 2 can write — and it can be built again where a delimiter needs it. The standard effects as programs
are in `okay.std.Programs`; `Program.run(top)(p)` runs one on a monad. A handler of a `Program` takes its effect off
WHEREVER it is in the row (`Removed`, okay's): the order of effects in a type says nothing, the order of handlers
everything. Your own effect is written once, in context — its handler for programs is one line,
`Program.handled(p)(c => body => yourHandler(using c)(body))` (an interceptor's, `Program.within`); or a
`Program.Handler`, written for programs, by `Program.handle(h)(p)` — its clauses over `Program.Ops`, plain methods
(`pure`, `flatMap`, `map`), so Scala 2 writes them too; a `Program.HandlerAt[O, …]`, by `Program.handleAt`, for a
clause that performs the effects of the row outside it, `O` (`ops.perform`).

```scala
type Row1 = Reader % Int +: Throws % String +: Pure

def checked: Program[Row1, Int] =
  Programs.ask[Row1, Int].flatMap(n => if n < 0 then Programs.raise[Row1, String, Int]("negative") else Program.pure(n * 2))

Program.run(okay.zero.top)(Programs.throws[String](Programs.reader(21)(checked)))   // Right(42)
Program.run(okay.cont.top)(Programs.throws[String](Programs.reader(-1)(checked)))   // Left("negative")
Program.run(okay.zero.top)(Programs.reader(21)(Programs.throws[String](checked)))   // Right(42)
```

### With tofu and cats

`Program.Of[R]` is an `F[_]`: with cats' monad over it and tofu's typeclasses (`WithLocal` from a `Reader` in the
row, `Errors` from a `Throws`), code written over `F[_]` with tofu runs on okay's effects, on any monad — the modules
`okay-core-cats` (`okay.interop.CatsProgram`, every platform) and `okay-core-tofu` (`okay.interop.TofuProgram`, the
JVM's): [docs/interop.md](interop.md).

### From Scala 2

The same artifact, read by scalac 2.13 with `-Ytasty-reader`; for Scala 2 the short names of `okay-core-scala2-prelude`
(`import okay.scala2._`: `ReaderOf[E]#L`, `Of[R]#L`) and the same cats and tofu instances compiled by scalac 2.13
(`okay-core-scala2-cats`, `-tofu`); the build's `scala2/tests` their proof, one's own handlers included. Scala 2 sees the classes and objects (`Program`, `Programs`, `In`, the
`top`s), not the top-level definitions; an effect of two parameters is a Scala 2 type lambda, and a row is written
without infix precedence:

```text
type App = +:[({ type L[X] = Reader[Int, X] })#L, Pure]      // or kind-projector's +:[Reader[Int, *], Pure]
val p: Program[App, Int] = Programs.ask[App, Int].map(_ + 1)
Program.run(okay.zero.top)(Programs.reader(41)(p))   // 42
```

## Async

`async`, `await` and `Async.run` — and asynchronous streams — are a module of their own, `async/` (package
`okay.async`): [docs/async.md](async.md). The core has no thread in it.

## Notes

- An effect the context does not have does not compile: there is no path to it.
- A program is a value: built once, run many times; `state` makes a fresh cell each run.
- Not every program is stack safe on every monad: `Cont` and `Cont0` are; the textbook reference in the tests is not.
