# Architecture

One interface, written once; several monads implementing it; references that define what they must do. `Cont0` is
the default; `Cont` a second implementation, different by design — what keeps the interface from being one monad's
API. No macros, no casts, no runtime tests of an operation's class.

```
okay.core   the interface          rows, Context, Control, Effects, Handler, intercept, Needs, A ! Es, Top,
                                   Program (a program over a row, built where it runs)
okay.std    effects, written once   Reader (local), State, Writer, Throws (handleWith), Choose, Emit, PState, Async;
                                   Programs (the same as Programs), Stream
okay.zero   the monad Cont0         shift0 and $, a machine               — At, run, top, shift0   (the default)
okay.cont   the monad Cont          shift/reset, a handler a fold         — At, run, top           (the second)
tests       the references          Ref (Cont0 without the machine), Cps0 (textbook CPS); tofu over Program
scala2/     Scala 2.13              the core read by scalac 2.13; tofu's 2.13 artifacts over Program
```

## What it is for, and the layers that follow

Four requirements, one design:

1. **A small kernel people can read and use on its own** — the core names no library; ~1 700 lines.
2. **The kernel of okay**, whose programs are `M[R, A]` — the row in the type — over `Effects[M]`, and whose
   handlers take an effect off the row wherever it is.
3. **Scala 2.13**, from the same artifact: okay reaches Scala 2 through scalac 2.13's TASTy reader (a cross-build
   of its own core was measured impossible: union rows, inline, opaque types); okay-core's rows are nominal, so the
   reader reads it as it is.
4. **Code written over `F[_]`** — cats, tofu: a carrier `F`, its typeclasses instances.

The layers:

- **The context (Scala 3).** A program is written against a context, `(using c: Effects)`, a value of
  `c.Prog[A]`: concise, the row and the answer never named, the monad the context's. A handler's body, a reset's,
  an interceptor's is a context function — which Scala 2 cannot write, and which `F[_]` code cannot take.
- **`Program[R, A]`, the row in the type.** A program over the row `R` NOT YET BUILT: `on(c)` builds it in any
  context whose row is `R`. It is `F[A]` for `F[_]` code (`Program.Of[R]`), okay's `M[R, A]`, and what Scala 2
  writes — a class with plain methods, `pure`, `flatMap`, `perform`, `run(top)`. Because it is built where it runs,
  it can be built again under a delimiter: what `local` and `handleWith` need, and what an already built `c.Prog`
  cannot give — its answer type is the one outside, so no delimiter goes around it. The standard effects as
  `Program`s: `okay.std.Programs`.
- **`intercept`, the one primitive added for them.** An effect already in the row handled again for a body: its
  operations at a path to the interceptor's clauses, the rest on as they are; a clause runs outside, so it reaches
  the handler. Paths compared by `In.same`, whose `Same` proves the two effects the same — no cast. On `Cont` a
  fold at the same row; on `Cont0` a `$` whose answers are read (`interceptRead`), the machine not entered —
  correct because inside it nothing is known of the handlers outside (`Ev.Unknown`), so an operation is never
  answered in place past the interceptor; the slow path, to be measured before a machine node is made. It is
  tofu's `WithLocal.local` and `Handle.handleWith`, and okay's handler of an effect anywhere in a row.
- **A handler of an effect anywhere in a row, okay's.** `Removed[E, R]` (okay's name): the row without `E`, `Out`,
  and every path in `R` translated to one in `E +: Out` (`Tr`). A handler of a `Program` builds its body in a
  `View` of the handler's context — a context seen over `R`, its operations' paths translated, everything else the
  context's own, its delimiters' contexts views too. `Programs`' handlers, and `Program.handle(h)(p)` with a
  `Program.Handler` (clauses written in any context outside). For it the interface's monad one level in is two
  members, not one indexed by a row: `Within` (a reset's, an interceptor's body) and `Handling[E]` (a handler's) —
  so a view keeps its row and takes the monad of the context it views. okay's own `TestEffectsRows`, over okay's
  interface `Effects[M[_ <: Row, _]]` written here over `Program` (`TestAnywhereOn`), passes on the four monads.
- **Integrations, outside the core.** tofu and cats instances over `Program` are in the tests now
  (`src/test/scala/okay/tofu`, `scala2/src/test/scala/okay/scala2`): the shape of an optional module, each
  dependency behind it.

Scala 2's view, measured: a Scala 2 program sees every class and object of the core (`Program`, `Programs`, `In`
and its givens as implicits, the monads' `top`s); it does not see top-level definitions — `%`, `:?`, `!`,
`perform`, the context-style `reader`, `ask` — and reads a type lambda alias as a proper type, so an effect of two
parameters is written as Scala 2 writes a type lambda, `({ type L[X] = Reader[Int, X] })#L` or kind-projector's
`Reader[Int, *]`; and its infix types have one precedence, so a row is `+:[A, +:[B, Pure]]` or parenthesised. An
`export` of a sealed trait crashed the reader (`addChild inapplicable for type Row`): `okay.zero` imports the rows
instead.

## The interface (`okay.core`)

### Rows (`Rows.scala`)

`Row`, `+:`, `Pure`; `In[E, R]`, the path to the effect `E` in the row `R`, built by the compiler (`Here`, or
`There` one effect further). Nominal, so every walk is total and needs no cast. `%` fixes an effect's first
parameter. `Sub`, `Req`, `Paths`: the paths of the effects a program needs (`A ! Es`), found in its context's row.

### The context (`Context.scala`, `Control.scala`, `Effects.scala`)

```scala
abstract class Context:
  type M[I, O, A]                 // a program here: value A, a continuation answering I, answering O
  type Effs <: Row                // the effects in force here
  type S                          // the answer at this point
  type Inner[X <: Row, I, O, A]   // the monad of the bodies of the delimiters here
  type Prog[A] = M[S, S, A]
  def pure, delay, flatMap, map

trait Control extends Context:
  type Reset[S2]  <: Control { M = Inner[Effs, …]; S = S2 }   // a reset's body
  type Answer[S2] <: Control { M = M; S = S2 }                // a shift's body
  def shift[I, O, A](f, same: (I =:= O) | Null): M[I, O, A]
  def reset[S2, O](body: Reset[S2] ?=> Inner[Effs, S2, O, S2]): M[S, S, O]

abstract class Effects extends Control:
  type Handled[E[_], S2] <: Effects { M = Inner[E +: Effs, …]; Effs = E +: Effs; S = S2 }
  type Body[E[_], Ans, A] = Handled[E, Ans] ?=> Inner[E +: Effs, Ans, Ans, A]
  def perform[E[_], S2, X](op: E[X], in: In[E, Effs]): M[S2, S2, X]
  def handle[E[_], A, Ans](h: Handler[Prog, E, A, Ans])(body: Body[E, Ans, A]): M[S, S, Ans]
```

- **The context is the instance** (final tagless). A program asks for `(using c: Effects)` and is a `c.Prog[A]`. The
  nearest given wins, so a delimiter gives its body a context of its own.
- **`Inner`, the monad one level in.** A handler's and a reset's body is in the monad of their bodies. For `Cont` it
  is the same monad. For `Cont0`, whose answers are programs of the level outside, it is one level deeper — the
  level is part of the type. This is what lets one interface describe both.
- **`Effects extends Control`.** A handler is a delimiter that answers the operations of its effect; a reset is one
  that answers none.
- **`same`, `Keeps`.** `shift` carries a proof that it keeps the answer (`I =:= O`) when the types say so
  (`Control.shift` finds it). `Cont` ignores it; `Cont0` needs it to run a handler's clauses inside `k`.
- **Handlers.** `Handler[P, E, A, Ans]`: `ret` and `apply(op, k)` over the programs `P` outside the handler.
  `Answering` adds `value(op)` and fixes `apply` to `k(value(op))`. `Value[E]` is what answering in place needs.
- **`Needs`, `Paths.Keep`.** The context of an `A ! Es`: an instance keeps the paths in a context of its own
  (`Paths.kept`), so an operation finds its path in one call.
- **`Top`.** A monad's context at the top, at any answer (`at[J]`), and a run of a program of it: what a library
  re-entering the top takes — `Async`, a run going on in a callback's thread; `stream`, a run of a generator, so its
  `Step` is one type whatever the monad. Every context has its monad's, `c.top`.

## `okay.cont` — `Cont`

`Cont[R, I, O, A]`: the monad of delimited continuations with answer-type modification as a tree — `Return`,
`Inject` (an operation), `Bind`, `Shift`, `Reset`, and for speed `Suspend` (an operation with its rest), `Map`,
`Delay`, `Push` (a resumed stack). `step` reads the tree to its head with a stack of frames (`loop0`, `loop`).

A handler is a **fold** over the body's tree (`fold0`, `fold`), made when `handle` is called: a value by `ret`; an
operation of its effect by its clause — answered in place for an `Answering` — with the rest of the body folded the
same way, lazily (`Resume`); another effect's forwarded outside, the rest re-folded (`Refold`); a `shift` folded in
place of the rest, the rest under a `Reset` of its own with `ret`. `At` is the instance.

## `okay.zero` — `Cont0` and the machine

### The kernel (`Cont0.scala`)

`Cont0[I, O, A]`: Materzok–Biernacki's shift0 and `$`. Four nodes — `Return`, `Bind`, `Shift0`, `Dollar` — open as a
monad of its own:

- `ret $ body`: the body's value through `ret`; its answers are programs of the level outside.
- `shift0(k => e)`: `k` the rest and the delimiter; `e` runs in the delimiter's place, outside it.

Under a delimiter the answers are **levels**, programs of the level outside yielding `Res`: the body's answer
(`Done`), an operation with its rest (`Req`), or a `shift` (`Ctl`). The levels nest in the answer types, so
`Eff[R, J, I, O, A] = Cont0[Level[R, J, I], Level[R, J, O], A]` — opaque, so no raw `shift0` or `$` can catch a
request, and no request is made up.

Every node the interface builds is a shift0 program, in one node each for speed: an operation `Op`
(`shift0(k => Req(op, k))`), `OpK` (an operation with its rest), a `shift` `Ctl`, a `shift0` `Ctl0`, a handler `Handle`
and a reset `Reset` (each a `$` whose answers are read), `Lift` (a program outside run there), `Resume` (a captured
rest resumed), `Map`, `Delay`. `step`, the kernel's loop, runs any of them; the machine's nodes by entering the
machine.

### The machine (`Machine.scala`)

Each handler's and reset's body runs in **a loop of its own** (`deliver`), to where it leaves the delimiter:

- **A value**: to the delimiter's answer by `ret`.
- **An operation**: answered **in place** if the handler at its path answers in place — found through `Ev`, the
  handlers in force, known at run time, so across any handlers and resets between. Otherwise it goes to the
  delimiter (`route`): a handler's own to its clause with `k` the rest (`Resumed`, lazy); another's out to the
  next delimiter, one handler shorter.
- **A resumption**: entered with its value going through its frames; `k(x)` from a shift's body is run nested in the
  body's loop (up to 64 deep, then out and back), as `Cont` nests a reset.
- **A `shift`** at its delimiter (Danvy–Filinski's): its body under the delimiter again, `k` the rest under it,
  lifted in. At a handler `k` holds the handler's clauses, so the shift must keep the answer — `same`; one that
  moves it is in a reset by its types, or goes to the definition.
- **A `shift0`** at its reset: the body is the program outside, `k` the rest under the reset.
- **Anything else** goes to the delimiter's **definition** (`bail`): the rest under a `$` whose answers are read
  (`handleRead`, `resetRead`) — the same semantics, slowly.

Delimiters come in two families: `ByReset` (`Reset`, and `Rst` made by the machine for a shift's body and its `k`) and
`ByHandler` (`Handle`, and `Hd`). A delimiter or a handler bound to a value (`Bind(Handle, f)`) is entered at once,
its value bound with no frame pushed.

### The instance (`At.scala`)

`okay.zero.At[R0, J0, T0]`: `M[I, O, A] = Eff[R0, J0, I, O, A]`; a delimiter's body one level deeper,
`Level[R0, J0, T0]`. `perform` builds an `Op`, `handle` a `Handle`, `reset` a `Reset`, `shift` a `Ctl`. `shift0` takes a
reset's context of `At` — a handler's row is one longer than its level's, so under a handler it does not compile.

## The references (tests)

- **`Ref`** (`zero/Ref.scala`): `Cont0` by the kernel alone, each delimiter its definition (`handleRead`,
  `resetRead`), no machine — what the machine must do. Shares the kernel with `okay.zero.At`; catches a machine
  that changes the meaning.
- **`Cps0`** (`core/Cps0.scala`): shift0 and `$` from the textbook in CPS, a program a function `(A => I) => O`, the
  same reading of answers; no code of `Cont` nor of `Cont0`. Catches a kernel that is wrong. Not stack safe: its
  tests run on a thread of a 2 GB stack.
- **`Cps`** (`core/TestControlCps.scala`): Danvy–Filinski's shift and reset in CPS, control only.

`Ref` and `Cps0` have `Cont`'s semantics for a shift directly under a handler; `okay.zero.At` differs there by
design, and `TestMonads` pins it.

## Tests

The suites are written once over the interface and take a `Backend` — a monad's run (`BackendSuite`):
`TestEffectsOn`, `TestContextOn`, `TestControlOn`, `TestTogetherOn`, `TestDynamicOn`, `TestPStateOn`, `TestAsyncOn`.
Each runs on `Cont`, `Cont0`, `Ref` and `Cps0`. Beside them: `TestCont` and `TestCont0` (the kernels), `TestMonads`
(the same programs on each monad, and where they differ), `TestShiftAtHandler` and `TestZero` (`Cont0`'s own),
`TestStreamOn`, `TestAsyncStreamOn` (streams on each monad's top), `TestInterceptOn` (`intercept`), `TestTofuOn`
(tofu over `Program`, okay.tofu), `TestTofuFromScala2` (scala2/: the same from Scala 2.13), `TestSealed` (what does not compile), `TestGuide` (the guide's examples),
`TestGuideDoc` (the guide's own text: its `scala` blocks generated into `GuideDoc` by `project/GuideDoc.scala`, each
`expr   // value` line checked), `TestAgree` (1 000 programs made at random — handlers nested in any order, resets and
shifts — on `Cont`, `Cont0`, `Ref` and `Cps0`, which must agree).

## Benchmarks

`sbt "bench -prof gc"` runs them and prints a table; `sbt "bench save -prof gc"` keeps the run in `bench.d`.

- `MonadsBenchmark` — the same programs on `backend=cont|zero`: binds, maps, tail calls, operations answered in
  place and by a clause, across 1, 4 and 16 handlers, the standard effects, `A ! Es`, generators, 8 queens,
  captures under a reset and under a handler.
- `DispatchBenchmark` — `Cont`'s own: what the interface costs against the concrete monad; a plain baseline.
- `ZeroShift0Benchmark` — `Cont0`'s `shift0`.

Where `Cont0` is slower — `choose`, `throws`, a shallow shift directly under a handler, by 3–17% — it is the price of
a delimiter being a node the machine enters: an entry, a frame stack, a resumption a little larger than `Cont`'s.
The same choice makes an operation across handlers 2–16× faster. Measurements and the changes behind them are in
`changelog.d` and `bench.d`.
