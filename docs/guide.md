# Guide

How to write programs with effects and delimited control, run them, and write effects of your own. The guide's own
text is compiled: `project/GuideDoc.scala` makes its `scala` blocks one object, and `TestGuideDoc` checks each line
`expr   // value` against the value written. `TestGuide` runs the examples besides.

```scala
import okay.*    // A ! E1 + E2, the standard effects, run — and for one's own: Handler, Answering, perform
```

## Programs and their place

A program is written for the **place** it will run at: `A ! E1 + E2` is a program of value `A` that needs the
effects `E1` and `E2` — in any order, in any row that has them among others, under any handlers. The row is never
named: the handlers around the program make it, and the program is fitted to whatever they make.

```scala
def greet: Int ! Reader % String + Writer % String =
  ask[String].flatMap(name => tell(s"hello, $name").map(_ => name.length))
```

- `flatMap` and `map` bind an operation into the row where the program runs; `pure(a)` is a value as a program;
  `defer(p)` builds `p` when it is run — a tail call in constant stack.
- `!`, `+` and `%` are all a program's type needs: `%` gives an effect of two parameters its first, `+` joins
  effects. An operation on its own (`ask[String]`) is not yet a program: it becomes one where it is bound, or named,
  `ask[String].at[R]`.

## Rows

```scala
type Row2 = Reader % Int +: Writer % String +: Pure   // a row: Reader of Int, then Writer of String
```

- `Pure` is the empty row, `E +: R` the effect `E` in front of the row `R`: the handlers in force, the nearest first.
- `F % S` gives an effect of two parameters its first: `Reader % Int` is the effect `[X] =>> Reader[Int, X]`.
- `E :? R` is the path to `E` in `R`; `Has[E]` the same as a bound: `def p[R <: Row: Has[Reader % Int]]: Eff[R, A]`
  is a program over any row named with `Reader % Int` in it — the kernel's own shape, `Eff[R, A]`.
- A program needing effects and one over a row named call each other freely — the row named one where the
  expected type says its row (not as the receiver of a `flatMap`: name it there, `named[R]`):

```scala
def named[R <: Row: Has[Reader % Int]]: Eff[R, Int] = ask[Int].at[R].map(_ * 2)
def needing: Int ! Reader % Int + Writer % String = tell("x").flatMap(_ => named)
run(writer[String, Int, Pure](reader(5)(needing)))   // (List("x"), 10)
```

## Running

A handler takes a body at its place — its effect at the head of the row outside — and gives a program of the row
outside. The outermost handler's row outside is `Pure`, and `run` runs a program over `Pure`:

```scala
run(reader("Ann")(writer[String, Int, Reader % String +: Pure](greet)))   // (List("hello, Ann"), 3)
run(writer[String, Int, Pure](reader("Ann")(greet)))   // the same
```

A handler's type arguments are its effect's parameter, its body's value and the row outside — `writer[W, A, T]`;
inferred where the expected type says them, written otherwise. Everything runs in constant stack: a loop of
100 000 operations is fine, handlers nested 100 000 deep too.

```scala
def counting(i: Int): Int ! State % Int =
  if i == 0 then get[Int] else get[Int].flatMap(x => put(x + 1)).flatMap(_ => counting(i - 1))

run(state(0)(counting(100000)))   // (100000, 100000)
```

## The standard effects (`okay.std`)

| Effect | Operations | Handler → answer |
|---|---|---|
| `Reader % E` | `ask[E]`, `local(f)(body)` | `reader(e)(body)` → `A`, answered in place |
| `State % S` | `get[S]`, `put(s)`, `modify(f)` | `state(s0)(body)` → `(S, A)`, each path of a choice its own; `cell(s0)(body)` — one cell a run, every resumption's |
| `Writer % W` | `tell(w)` | `writer(body)` → `(List[W], A)` |
| `Throws % E` | `raise(e)`, `handleWith(body)(f)` | `throws(body)` → `Either[E, A]` |
| `Choose` | `among(as)`, `fail` — multi-shot | `choose(body)` → `Seq[A]`, every path; `first(body)` → `Option[A]` |
| `Emit % W` | `yield_(w)` | `collect(body)` → `(List[W], A)`; `stream(source)` → `Generator[W]`, pushed or pulled; `generate(next, done)(body)` — lazy |
| `Shifts[S, T]` | `shift(k => …)`, `shift0(k => …)` | `reset[S, T](body)` → `S`: delimited control as an effect of the row |

The order of handlers is the semantics: `state(throws(body))` keeps the state of a failed body,
`throws(state(body))` loses it.

```scala
def pairs: (Int, Char) ! Choose = among(Seq(1, 2)).flatMap(n => among(Seq('a', 'b')).map(ch => (n, ch)))

run(choose(pairs))   // Seq((1,'a'), (1,'b'), (2,'a'), (2,'b'))
```

## An effect of your own

An effect is an enum of its operations, each with its value's type; an operation is a program by `perform`:

```scala
enum Log[A]:
  case Line(s: String) extends Log[Unit]

def log(s: String): Op[Log, Unit] = perform(Log.Line(s))
```

A handler gives the body's value its answer (`ret`) and each operation its clause, with `k`, the rest of the body
up to the handler — data: called once, twice (multi-shot, as `choose`), never (as `throws`), or kept. Handlers are
deep: `k` runs under the same handler again. A handler is over any row outside, `T`; `handle(h)(body)` puts it at
the head of one.

```scala
def logging[A, T <: Row](body: Under[Log, T, A]): Eff[T, (List[String], A)] =
  handle(new Handler[Log, A, (List[String], A)]:
    def ret(a: A): (List[String], A) = (Nil, a)
    def apply[R <: Row, X](op: Log[X], k: X => Eff[R, (List[String], A)]): Eff[R, (List[String], A)] = op match
      case Log.Line(s) => k(()).map((lines, a) => (s :: lines, a)))(body)
```

- `Under[E, T, A]` is the handler's body: a program of value `A` at the handler's place, `E` in front of the row
  outside `T` — what an `A ! Es` program is fitted to; built when the run enters the handler.
- `k` is lazy: calling it builds the rest, running it is up to the clause.

### Answering in place

A handler whose every clause is `k(value(op))` — it only gives the operation a value — should be an `Answering`. Its
operations are then answered where they are performed, nothing captured, even across other handlers.

```scala
enum Clock[A]:
  case Now() extends Clock[Long]

def now: Op[Clock, Long] = perform(Clock.Now())

def fixed[A, T <: Row](t: Long)(body: Under[Clock, T, A]): Eff[T, A] =
  handle(new Answering[Clock, A, A]:
    def ret(a: A): A = a
    def value[X](op: Clock[X]): X = op match
      case Clock.Now() => t)(body)
```

```scala
def prog: Long ! Log + Clock = now.flatMap(t => log(s"at $t").map(_ => t * 2))

run(logging(fixed(21)(prog)))   // (List("at 21"), 42)
```

### Handlers are dynamic

A program's operation goes to the handler it *runs* under, not the one it was built under: a generator's rest,
returned out of its `reader(7)` and run under `reader(8)`, asks 8.

## Effects from effects

The standard effects are written with the same API as one's own, so whatever is said here of them is said of any.
Four ways to build on effects there are, from the least to the most:

**Operations of existing effects, no effect of one's own** — a program over a row with them, as `modify` is `get`
and `put`:

```scala
def incr: Int ! State % Int = get[Int].flatMap(n => put(n + 1).map(_ => n + 1))

run(state(0)(incr.flatMap(_ => incr)))   // (2, 2)
```

**An effect of one's own, translated to another outside it** — `translate(f)(body)`, a handler whose every clause
is a program over the row outside, `f(op)`, the rest of the body then resumed with its value. The effects it needs
are in the row outside, their paths in its signature:

```scala
enum Audit[A]:
  case Note(s: String) extends Audit[Unit]

def audit(s: String): Op[Audit, Unit] = perform(Audit.Note(s))

def auditToWriter[A, T <: Row](body: Under[Audit, T, A])(using Writer % String :? T): Eff[T, A] =
  translate[Audit, T, A]([X] => (op: Audit[X]) => op match
    case Audit.Note(s) => tell(s"[audit] $s").at[T])(body)

def audited: Int ! Audit = audit("a").flatMap(_ => audit("b")).map(_ => 7)

run(writer[String, Int, Pure](auditToWriter(audited)))   // (List("[audit] a", "[audit] b"), 7)
```

- The effect invariant, `enum Audit[A]`: a match on its operations then knows the value's type exactly.
- A clause needing the rest itself — run twice, never, or not last — is a `Handler` (*An effect of your own*);
  one made of the effects outside, with `k`, the kernel's `Deep[E, T, A, B]` over the row `T`, by `handleIn`.

**An effect of one's own on another installed inside, hidden.** The caller sees `Ticket` only; the state is the
handler's own:

```scala
enum Ticket[A]:
  case Next() extends Ticket[Int]

def ticket: Op[Ticket, Int] = perform(Ticket.Next())

case class Issued(n: Int)   // a type of the handler's own: no one outside names it

def tickets[A, T <: Row](body: Under[Ticket, State % Issued +: T, A]): Eff[T, (Int, A)] =
  state(Issued(0)):
    translate[Ticket, State % Issued +: T, A]([X] => (op: Ticket[X]) => op match
      case Ticket.Next() => modify[Issued](i => Issued(i.n + 1)).map(_.n))(body)
  .map((i, a) => (i.n, a))

def twoTickets: Int ! Ticket = ticket.flatMap(a => ticket.map(b => a * 10 + b))

run(tickets(twoTickets))   // (2, 12)
```

- The hidden effect's type is private. A `State % Int` there would be the one the body's own `get[Int]` finds, the
  nearest; `State % Issued` no one outside can name, so the caller's state is the caller's:

```scala
def mixed: (Int, Int) ! State % Int =
  tickets(ticket.flatMap(a => ticket.map(b => a + b))).flatMap(r => incr.map(_ => r))

run(state(100)(mixed))   // (101, (2, 3))
```

**An existing effect changed for a part of the program** — an interceptor (below, *Intercepting*): the kernel's
`Deep` over the row outside, its clause passing the operation on, changed:

```scala
def shouting[A, T <: Row](body: Under[Writer % String, T, A])(using Writer % String :? T): Eff[T, A] =
  intercept(new okay.one.handler.Deep[Writer % String, T, A, A]:
    def ret(a: A): Eff[T, A] = pure(a)
    def apply[X](op: Writer[String, X], k: X => Eff[T, A]): Eff[T, A] = op match
      case Writer.Tell(s) => tell(s.toUpperCase).at[T].flatMap(_ => k(())))(body)

def partly: Unit ! Writer % String = tell("a").flatMap(_ => shouting(tell("b").perform))

run(writer[String, Unit, Pure](partly))   // (List("a", "B"), ())
```

## Delimited control

Two faces, each where its typing is free.

**As an effect of the row, `Shifts[S, T]`** — `reset[S, T](body)` delimits, `shift(k => …)` captures the rest up
to the nearest reset as `k`, the answer `S` fixed, `T` the row outside the reset. Under any handlers, in any order;
a shift directly under a handler holds the handler in its `k`:

```scala
def twice[T <: Row]: Eff[T, Int] =
  reset[Int, T](shift[Int, T, Int](k => k(10).flatMap(k)).at[Shifts[Int, T] +: T].map(_ + 1))

run(twice[Pure])      // 12
run(reader(3)(twice[Reader % Int +: Pure].flatMap(x => ask[Int].at[Reader % Int +: Pure].map(_ + x))))   // 15
```

- `k` is the rest *with* the reset (Danvy–Filinski): `shift`'s body runs under the reset again, `shift0`'s outside
  it, in its place.
- Effects and control compose: a `reset` passes every operation on; a shift's body and `k` may perform.

**The answer moved, `Control[R]`** — `shift0` and `$` with the answer type modified (Materzok–Biernacki, typed as
Asai–Kameyama): the kernel's own nodes, `okay.one.Cont.{Shift0, Dollar}`, for printf and the like; a program of
effects enters control by `cont`, a closed program of control is one of effects again by `lower`. See
`TestControl`.

## Streams

A **source** is a program that yields: `Source[W]`, under `Emit % W` alone. `stream(source)` makes it a
`Generator[W]` — a `Stream[W]` of its elements, not started. A value: each consumption runs it from the beginning.

```scala
def fibs(a: Long, b: Long): Unit ! Emit % Long = yield_(a).flatMap(_ => fibs(b, a + b))

stream[Long](fibs(0, 1)).take(8)   // List(0, 1, 1, 2, 3, 5, 8, 13)
```

Every `Stream[W]` is consumed the same way:

- **pushed** — `foreach`, `fold`, `toList`: the body run to its end, each element handed on where it is yielded.
  For a stream that ends; the fastest pass.
- **pulled** — `take(n)`, `iterator`: as far as asked; the rest never runs. `iterator` is closeable: `close()`
  ends the run where it is.
- `map`, `filter`, `flatMap`, for-comprehensions; `Stream.of(ws*)`.

```scala
(for n <- stream[Long](fibs(0, 1)) if n % 2 == 0; s <- Stream.of(n, -n) yield s).take(6)   // List(0, 0, 2, -2, 8, -8)
```

A generator's `map` and `filter` are fused: composed into what its handler does with an element, no object an
element of their own. `start` runs a generator to its first element, a `Step[W]`: `Next(w, rest)` (the rest a
program over nothing, `k` as data) or `Done()`; `pull()` runs the rest, as often as asked — a rest pulled again
gives the same elements again. A `Step` made from another (`map`, `flatMap`, `++`, `Step.of`) has a third case,
`Later(w, rest)`, its rest a function.

Effects the source handles itself are in the stream: `stream(reader(5)(body))`. Under effects still to be handled,
`generate(next, done)(body)`: a generator of the consumer's own type.

## Intercepting: `local`, `handleWith`

An effect already in the row can be handled again for a part of the program: `intercept(h)(body)` — the body's
operations of that effect go to `h`'s clauses, `k` the rest under the interceptor again; a clause runs outside it,
so its own operation of the effect reaches the handler. The standard ones:

```scala
def scaled[T <: Row: Has[Reader % Int]]: Eff[T, (Int, Int)] =
  ask[Int].at[T].flatMap(a => local[Int, Int, T](_ * 10)(ask[Int].perform).map(b => (a, b)))

run(reader(4)(scaled[Reader % Int +: Pure]))   // (4, 40)
run(throws[String, Int, Pure](handleWith[String, Int, Throws % String +: Pure](raise("bad").perform)(e => pure(e.length))))   // Right(3)
```

Nested ones compose, as tofu's `local`s do: the inner clause's `ask` is the outer interceptor's.

## `Eff[R, A]`: the row in the type, and okay's interface

`Eff[R, A]` is the kernel's program over a row named, `okay.one`'s `A ! R` — what code written over `F[_]` takes
(`Eff[R, *]`), what okay's `A ! R` is, what Scala 2 can write. The kernel's own handlers take their effect off
WHEREVER it is in the row (`Removed`, okay's): the order of effects in a type says nothing, the order of handlers
everything — `p.handle(State(41))`, `okay.one.std`'s. `RowEffects[Eff]` is okay's `Effects[M]` over it.

```scala
type Row1 = Reader % Int +: Throws % String +: Pure

def checked: Eff[Row1, Int] =
  ask[Int].at[Row1].flatMap(n => if n < 0 then raise("negative").at[Row1] else pure(n * 2))

run(throws[String, Int, Pure](reader(21)(checked)))   // Right(42)
run(throws[String, Int, Pure](reader(-1)(checked)))   // Left("negative")
```

## Async

`async`, `await` and `Async.run` — and asynchronous streams — are a module of their own, `async/` (package
`okay.async`): [docs/async.md](async.md). The core has no thread in it.

## Notes

- An effect the place does not have does not compile: there is no path to it.
- A program is a value: built once, run many times; `cell` makes a fresh cell each run, `state` is a register of
  the machine's.
- Everything runs in constant stack: operations, binds, handlers nested, resets.
