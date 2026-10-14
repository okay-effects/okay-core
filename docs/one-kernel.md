# The kernel, `okay.one`

THE ACCOUNT OF THE KERNEL, as its author wrote it in the okay-one repository (its README, at 45189cb) — the kernel
lives in okay-core since 2026-10-14, this page with it. An effect framework and its kernel: **effects with rows in the monad of shift0 and `$`**, the boundary between the
framework and the kernel data. No dependencies of its own; the facade (`okay.core`, docs/architecture.md) is over it.

- `row` — the rows of effects: `Pure`, `E +: T`; an effect's place `In[E, R]`; `Removed[E, R, T]`, a handler taking
  `E` from any place; `Control[T]`, the effect of a `$` — in the row as `Control[T] +: T`, `T` the row outside it;
  `Nearest[T, R]`, a shift0's path to the nearest `$`. Types only.
- `Cont` — a program as data: the monad's nodes (`Return`, `Bind`, `Map`, `Defer`, `Raise`), effects' (`Perform`,
  `Handle`, `Resume`; a program of effects at any answer, `Lift`), control's (`Shift0`, `Dollar`; `Lower` —
  `reset(body)`, a `$` whose answer is a value).
- `handler` — the forms of a handler, values, each over the row `T` outside it — its `ret`, its clauses and `k`
  programs over `T`. Those leaving the rest where it is, the answers of the program passing through them — so at any
  answers, a `$` around them, a shift0 under them taking them into its `k`:
  - `Answering`, `Stateful` — each operation answered in place, its value at once; a state taken with each `k`;
  - `Translating` — each operation a program over the row outside, run where it is performed: no `k`, no delimiter;
  - `Linear` — a clause given `k`, at any answers: resumed once, the types have it, before and after it anything.
  And those taking the rest — on a program of effects only, with no answer to drop:
  - `Aborting` — the rest dropped;
  - `Deep` — a clause given `k`: **data**, a program over `T` — dropped, resumed, resumed again, kept in the answer
    and resumed after the handler has answered, anywhere `T` is handled.
- `machine` — the kernel: a CEK machine over **one `Meta`** — the handlers' levels and the `$`s (`Dollar`, `Reset`),
  a program of effects at an answer (`Lifted`) — one loop over it, typed by the answers. A handler is found by an
  operation's place, past `$`s as past any effect: answering in place, the levels between untouched; else `k` built
  on the operation's way out, a level at a time, the `$`s in it, put back where it is resumed. A shift0 goes out the
  same way, by its path, to its `$`: the handlers between in its `k`.
- `step` — a program stepped: run to its value or to its first operation of the row outside, given back with its
  rest, `k` as data (`Done | Yield(op, in, resume)`) — what a driver outside is built on.
- `std` — `Reader % V`, `State % S`, `Writer % W`, `Throws % E`, `Choose`; `Shifts[S, T]`, delimited control as an
  effect — `reset` its `Deep` handler, `shift0` and `shift` (Danvy–Filinski's) its operations; `Emit % W` — `collect`,
  and `generate`, a lazy generator of the consumer's own type, `k` kept as its next step; `PState`, a state whose
  type changes, carried by the answer type under a `$`; `Async`, driven by the program stepped.

```scala
infix type ![+A, R <: Row] = Cont[R, Nothing, Nothing, A]   // a program of effects: no answers of its own
type Delim[T <: Row] = Control[T] +: T                        // the row under a `$`, `T` outside it

def asked[R <: Row](using In[Ask, R], In[Log, R]): Int ! R =
  for n <- perform[Ask, R, Int](Ask.Get); _ <- perform[Log, R, Unit](Log.Tell(s"got $n")) yield n + 1

asked[Ask +: Log +: Pure].handle(reader(41)).handle(log).run        // (List("got 41"), 42)

def choose[T <: Row, A] = new Deep[Choose, T, A, List[A]]:
  def ret(a: A) = pure(List(a))
  def apply[X](op: Choose[X], k: X => List[A] ! T) = op match
    case Choose.Flip => for a <- k(true); b <- k(false) yield a ++ b
    case Choose.Fail => pure(Nil)

/** k kept: a generator, its next value and its rest */
enum Gen[T <: Row]:
  case Done()
  case More(v: Int, next: Unit => Gen[T] ! T)

def generate[T <: Row] = new Deep[Emit, T, Unit, Gen[T]]:
  def ret(a: Unit) = pure(Gen.Done())
  def apply[X](op: Emit[X], k: X => Gen[T] ! T) = op match
    case Emit.Value(v) => pure(Gen.More(v, k))
```

`p.handle(h)` takes `E` from any place in the row; `h`'s row `T` found from it — `handle`'s `T` has no bound, so the
compiler finds it by `Removed`, not fixing it to `Row` first.

**Control is an effect of the row**, `Control[T]`: a `$` over `T` puts `Control[T] +: T` around its body, as a handler
puts its effect; the answer types `I`, `O` of a program are its state, the nearest `$`'s; a program over a row with
none has none of its own. Its operation, `shift0`, is a node — it moves the answer, which an operation cannot; its
path to the nearest `$`, `Nearest[T, R]`, is found as `In` is, past the handlers before the `$`, never past a `$`:
its `k` is a program over `T`, the row outside, the handlers between in it — a state as it was when `k` was taken,
each time it is resumed. The operations of `T` go out past the `$` to their handlers by `In.There`, as past any
effect; a handler takes its effect from before the `$`, never past it. A `$` over a row of effects is a `reset`: its
answer a program over the row, run outside it, or a value (`reset`).

```scala
// printf with a state under the `$`: the state in `k`, each call of the function from the state as it was
type S = Int => String
type Q = St +: Reset                                   // a counter, then the `$`, nothing outside
val body: Cont[Q, String ! Pure, S ! Pure, String] = for
  n <- perform[St, Q, Int](St.Get)                           // at the answer of its rest, S ! Pure: lifted there
  x <- rows[Q].shift0[String, Int](k => pure((x: Int) => k(x).run))   // the answer turned String into S; k over Pure
  m <- perform[St, Q, Int](St.Get)                           // after the shift0 the answer String ! Pure
yield s"$n $x $m"
val f: S = dollar(body.handle(counter(0)))((s: String) => pure("<" + s + ">")).run
f(42)                                                        // "<0 42 1>", and again the same
```

An operation of the row has no answer of its own: bound in a program moving the answer, it is put at the answer of
its rest — `flatMap` is `inline`, the answers met decided at compile time (`Lifts`): the same, a bind as it is; a
program of effects, lifted (`Lift`, `p.cont[S]` by hand). `rows[R].shift0[X, A](k => …)`: `X` the value of the rest, `A` the shift0's own,
`k` and the body programs of effects over the row outside the `$`; the general form, `Shift0(at, f)`, for a `k` into
a `$` whose answer is a program.

What the types refuse: `Aborting` or `Deep` where a shift0 under them changes the answer — the answer is the
shift0's to make, and a handler dropping the rest, or resuming it twice, has none to give — so those only on a
program of effects; a handler of `Control`; a shift0 past a `$`.

**Failure is a node**: what user code throws — a bind's, a map's, a clause, a `value`, a `ret` — is caught at its
call, never around the loop, and the machine goes on with `Raise(e)`: the frames of each level dropped, the levels
unwound innermost first, the top's answer `Step.Failed(e)`; `run` rethrows.

**Guards**: a level of no effect around a program — `p.ensure(fin)`, `p.recover(f)`, `p.uncancelable`, `p.guard(g)`.
Unwinding runs each guard's finalizer where the guard was, innermost first, and gives the error to the first
`recover` (an error thrown, not an operation's raise). An abort past guards (`Aborting`) unwinds to its handler's
level, each finalizer run on the way. A guard taken into a `k` is put back each resumption: its finalizer once a
resumption. At a step, `resume.fail(e)` raises inside the rest's levels; `resume.cancel()` unwinds them — finalizers
run, no recover asked, `Step.Cancelled`; under `uncancelable` the step is given back as it is, the cancel fired when
the program leaves the mask.

**The specification**: `TestReference`, the whole monad's meaning, each node by its rule — effects: the textbook
substitution semantics of deep handlers over the nodes, a program's normal form its value or its first operation with
the rest; control: continuation-passing style. The machine is checked against it on random programs, their answers
pinned by a digest. `TestGuarantees`: what the types refuse. No casts, no unchecked type test (`-Werror`).

## Speed

µs, JMH on one machine, against okay-eff (the machine this one's is) and okay-core-zero (okay-core's kernel before this one), the kernel's
own lanes (`KernelBenchmark`, src/jmh, no facade):

| | okay-core-zero | okay-eff | the kernel |
|---|---|---|---|
| an operation answered in place (`answered`) | 5.5 | 4.8 | 5.4 |
| across 16 handlers (`forwarded16`) | 5.7 | 17.3 | 5.7 |
| a state (`state`) / each path its own under choose (`choose`) | 12.9 / 24.3 | 11.4 / 13.4 | 12.1 / 13.1 |
| a `Deep` handler at each operation (`general`) | — | 8.6 | 9.4 |
| a shift under a reset, the rest 16 maps, `k` 8 times (`capture`) | 0.30 | — | 0.95 |
| the same by shift0 / by the machine's own `$` and shift0 nodes | — | — | 0.73 / 0.92 |
| a generator pulled, an element a run (`generate`) | 13.6 | — | 22.6 |

**Behind on capturing `k`, two to three times — the price of `k` as data.** A `k` resumed puts its levels back on
the `Meta` where it is resumed, each a new object, and runs its frames again; okay-core-zero resumes in place, its
nested delimiters on the JVM's stack. Taking the old levels back when `k` is resumed where it was taken would want a
proof the types cannot give (a cast). Measured and not kept: four maps a step at a resumption, maps fused as they are
pushed — each faster on `capture`, each 12–75 % slower on maps run once (`leftMaps`, `rightMaps`). The machine's own
`$` and shift0 are no faster than `Shift % S` by a handler: the cost is resuming, not the handler. (Measured on
c398fee, the one loop typed by the answers.)

## Where it is checked

Its suites are `src/test/scala/okay/one` (`TestReference` the meaning, `TestGuarantees` what the types refuse, the
rest by node and by std); they run on a 512k stack. Its lanes: `sbt "bench KernelBenchmark"`.
