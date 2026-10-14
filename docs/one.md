# okay-core on one monad: the landing plan

Decided 2026-10-13, after a week of experiments beside this repository (okay-zero, okay-free, okay-eff, okay-cont,
okay-one — each a record of what it found, see §1):

1. **okay-core is the kernel**: the reference small, independent, optimal kernel of effects, on which everything
   else — okay, okay-kernel's protocol, okay-java's adapter, the interop modules — runs. Not a library of two monads
   under an interface: **one monad, one machine**.
2. **A handler's `k` is data**: a program over the row outside the handler — stored, resumed any number of times,
   anywhere that row is handled (`shift(identity)`; `generate` keeps `k(())` as the next step of a stream). Not a
   token that cannot leave its clause.
3. **Platforms wait**: JS, Native, Scala 2 and the interop modules are re-based last; the architecture is settled on
   the JVM first.

This page is the plan: what the one monad is (§2), what of okay-core stays and what goes (§3), what the kernel must
prove before it moves in (§4), the order of the landing with its gates (§5), and what is deferred (§6).

## 1. Where the one monad comes from

| | found | kept |
|---|---|---|
| okay-core (`okay.cont`, `okay.zero`) | one interface over two monads keeps the interface honest, and costs a context-function language (`Context`, `Effects`, `Handled ?=>`) no second monad will ever need again | the facade `!`/`+`/`%`, the std's names, `Stream`, `RowEffects`, the kernel bridge, the suites written once |
| okay-zero | `Cont` as a kernel of four rules, effects as an extension above it (`Inject`, `ContVM`) — fast captures, but two loops, two sets of registers | `MonadsBenchmark`'s programs as the common gauge; the three state programs (§4 D) |
| okay-free | the answer types `I, O` in the effects' monad poison every frame and handler; handlers split into those a `shift0` may pass and those it may not | nothing of the encoding; `TestLaws`, `TestEffGuarantees` as the shape of a guarantees suite |
| okay-eff | `Eff[R, A]` with no answers; `k` as data (`Cont`: the levels passed, outermost first, around the rest); a CEK machine with the handlers in force a typed stack (`Meta`), an operation dispatched by its path through it. The fastest on the lanes where `k` works hardest: choose 12.9 µs, state 11.4, general 8.8. The lab: effects as levels of the CPS hierarchy — the wrong unit (a handler must be a thing the machine knows; a row a thing the types carry for the program) | the machine, the handler kinds, `k` |
| okay-cont | rows *inside* `Cont[R, I, O, A]`: `R` a row of effects or `Control[R]`; a program of effects has no answers of its own, answer types move only in control; `TestReference`, the whole monad's meaning in CPS checked on random programs | the type; control in the same monad; the reference as the specification |
| okay-one | the framework and the kernel with their boundary **data**: `Cont` nodes, handler forms as values, the machine an interpreter of them; `Step` (a program run to its value or its first operation of the row, given back with its rest); `Translating` | the layout; `Step`; the five handler forms |

okay-one is where the kernel is being written now, with okay-eff's machine under okay-cont's type: `Deep.apply[X](op: E[X], k: X => B ! T)`, `Cont` the levels as data, `Meta` the handlers in force.

## 2. The one monad

```scala
enum Cont[R, I, +O, +A]:                    // a program of A over R; I the answer of its rest, O what it answers
  // the monad, over any R
  case Return(a) / Bind(m, f) / Map(m, f) / Defer(c)
  // effects, over a row R <: Row: answers unmoved (A ! R = Cont[R, Nothing, Nothing, A])
  case Perform(op, in) / PerformHere(op)     // an operation, its place in R (none carried at the head)
  case Handle(h, p, rm)                      // p with E handled by h, E taken from any place in R (Removed)
  // delimited control, over Control[R]: the answer type modified (Materzok–Biernacki; Asai–Kameyama's typing)
  case Shift0(f) / Dollar(body, ret)
  case Lift(p) / HandleIn(h, c, rm)          // a program of effects in control; an in-place handler around control
  // the machine's own: Resume (k's levels put back, then its rest), and what it fuses (Abort, Clause, …)

sealed abstract class Handler[E[_], T <: Row, A, B]     // E handled; T the row outside; A in, B out
  Answering:   value(op): X                              // in place, no k
  Stateful:    initial; value(s, op); next(s, op); ret(s, a)   // a register of the machine; each k a snapshot
  Translating: apply(op): X ! T                          // a program outside, run where the operation is; no k
  Aborting:    abort(op): B ! T                          // the rest dropped, no k
  Deep:        apply(op, k: X => B ! T): B ! T           // k data: dropped, resumed, resumed again, kept
```

- **Rows** are nominal (`E +: T`, `Pure`); an operation carries its path (`In`), a handler its `Removed`; a program
  over "any row with `E`" is `R <: Row: Has[E]`. No union, no cast, no class test of an operation.
- **A program of effects never holds a `shift0` or a `$`**; `Control[R]` holds them, with the effects of `R`
  inside answered in place only (`HandleIn` takes `Answering`; `Deep`/`Aborting` refused by the types — their
  delimiter would be one among the program's, reached by its `shift0`s). A program of effects enters control by
  `cont` (`Lift`).
- **Delimited control as an effect of the row** — okay's `shift`/`shift0`/`reset` under any handlers, in any
  order — is `Shift % S`: an effect with a fixed answer `S`, `reset` its `Deep` handler, `shift` its operation with
  `k`. `shift(identity)` is `pure(k)` with `S` holding it. `Control[R]` is for the answer *moved* (printf,
  `PState`). Two faces of control, each where its typing is free.
- **`Lower`, the way back from control to effects**: `reset[S, O](body: Cont[Control[R], S, O, S]): O ! R` — a
  closed program of control (its value its initial answer, the rest the identity) is a program of effects over `R`.
  Inside it, an operation of `R` not answered in place goes **out**, to the handlers of effects around the reset;
  a `Deep` handler's `k` taken past the reset holds the `$` as a level and puts it back when resumed. So
  `choose(reset(printf))` and `attempt(reset(…))` are programs; `reset(choose(…))` is refused by the types, as
  before. This is what makes `$`s levels of the **same** `Meta` as handlers — one machine, not a machine of
  control beside a machine of effects: a `$` is a kind of level, a `shift0` goes out to the nearest `$`-level past
  no handler-level (the types see to it), an operation goes out past `$`-levels to its handler-level.
- **Failure is a node, not an exception in flight**: a user function (`Bind`'s, `Map`'s, a clause, `value`) that
  throws is caught at its call site — a `try` with nothing thrown costs nothing — and the machine goes on with
  `Raise(e)`: the frames of the level dropped, the levels unwound innermost first; at the top `Step.Failed(e)`
  (`run` rethrows). A `Stateful` register is never left behind by an exception flying through `Meta`.
- **`Guard`, the level with no effect** — `ret(a)`, `finalizer: Unit ! T`, `recover(e): B ! T`: `ensuring`,
  `recover` and `uncancelable` are its three instances. Unwinding runs each `Guard`'s finalizer and gives `e` to
  the first `recover`; an `Aborting` handler's drop of the levels between unwinds them too, so `attempt` runs the
  finalizers it skips. `Cancelled` is an unwinding no `recover` catches, started only at a suspension —
  `Step.Yield.cancel()`, the protocol's `Resume.cancel` — and, under `uncancelable`, kept pending until the level
  is left (the `Yield` given back as it is, its `resume` to cancel at the mask's exit). With `k` as data a `Guard`
  taken into a `k` is put back each resumption: its finalizer runs once per completion or abort of each
  resumption (Koka's); a `k` never resumed runs none — abort is `Aborting`, not a `k` not called; okay's `Resource`
  is linear, said so.
- **The machine** is one `@tailrec` CEK loop, `eval(c, k, m)`: `c` the node, `k` the frames of the level, `m` the
  levels outside — a handler each (its state, if `Stateful`, a register there), or a `$`. An operation answered in
  place goes straight into the frame on top, the levels between untouched; else out past each level, each put
  around its rest in a `Rest`; `k(y)` a `Resume` node, the levels put back on **the `Meta` in force where it is
  resumed**, outermost first, then the rest. No cast; no exception at run time for a program that type checks;
  constant stack at any depth of handlers, binds, resets.
- **`Step[R, A]`** = `Done(a) | Yield(op, in, resume) | Failed(e)`, `resume` with `apply(x)`, `fail(e)`,
  `cancel()`: the top-level driver's face — `Async`, replay,
  a pull stream, a foreign monad, and the kernel protocol's `Computation`/`Yield`/`Resume` almost verbatim.
- **`TestReference`**: the meaning of every node in CPS, nothing clever; random programs run by both.

## 3. okay-core: what stays, what goes

Two monads are 1 000 lines (`zero/` 834, `cont/` 526); 43 files in every module name one of them. What matters is
not those lines but the interface they share.

**Goes.** `okay.core.Context`, `Effects`, `Control`, `Top`, `At` — the context-function interface a program was a
function of (`(c: Effects) ?=> c.Prog[A]`, `c.Body`, `Handled ?=>`, `Within`, `needing(Paths)`); `View`;
`okay.cont`, `okay.zero`, `ContKernel`, `ZeroKernel`; the test backends `Backend`, `Cps0`, `Spec`, `TestAgree`,
`TestMonads` (where the two differed by design — one monad differs from nothing), `cont0-vs-cont.md`.

**Stays, re-based on `A ! R`.** The facade `Rows` — `!`, `+`, `%`, `Needs`/`Place`: `A ! E1 + E2` is all a
program's type says, the row made by the handlers around it, fitted where it runs — now `Needs` over the kernel's
`In`/`Removed` evidence, not over a context. `okay.std` by its names (`reader`, `local`, `state`, `cell`, `writer`,
`attempt`, `handleWith`, `choose`, `collect`, `generate`, `stream`, `pstate`). `Stream` (250 lines, over
`generate`: `k` as data is what it is made of). `RowEffects`, okay's `Effects[M]` instance, over `A ! R` directly —
`Program` (a program built at its row) is no longer needed under it. `okay.core.kernel.OnCore` through `Step`, one
bridge, not one per monad. `async/`, `interop/`, `scala2/`: unchanged in intent, last in order (§5). The guide, its
examples compiled by `TestGuide`; `bench.d`, okay-core-zero's numbers kept as the base the one monad is measured
against.

**Decided here, to be held to.** The facade is **over** the kernel, not in it: the kernel's surface is `A ! R`,
`perform` by `In`, `handle` by `Removed`, `Has[E]` as a bound; `Needs`/`Place` cost one object a run and belong to
the layer okay compiles on. okay-eff dropped them for the same reason.

## 4. What the kernel proves before it moves in

Programs with their answers, each a test the kernel passes in okay-one first; most exist in this repository's
suites already and are named by them.

- **A. `k` as data.** `generate(next, done)(body)` as `std/Emit.scala` has it: a `Deep` clause `pure(next(w, k(())))`
  — the rest of the body kept as the next step of the consumer's own type. `TestStream` 17: a rest pulled again is
  run again, the same elements (multi-shot from data). `TestStream` 72: a state handled in the body lives in the
  stream, one cell every pull shares. A `k` resumed under *another* handler of its outer row — zip of two
  generators, each `k` resumed inside the other's run: operations of the row outside go to the handlers in force
  where it is resumed.
- **B. `Shift % S`, control as an effect.** `TestControl`'s six (abort, multi-shot, a shift in a shift's body
  captures to the same reset, nearest reset only, `k` runs nothing until stepped, 100 000 resets in constant
  stack). `TestShiftAtHandler`'s seven: a shift directly under a handler, no reset between — `k` holds the handler
  (`Cont`'s semantics, the one okay keeps): `k(a) = (3 + a) * 10`; tells in the body, in `k`, after the shift seen
  in order by the writer outside. `TestMonads` 80: an answer moved and moved back directly under a handler.
- **C. `Control[R]`, the answer moved.** `TestPState`'s four — `get` after `put` the type unchanged; an `Int` in, a
  `String` put, read back a `String`; a `get` of the wrong type does not compile; an operation of the row between
  the moves (an in-place handler inside control). `TestControlCps`: `Cont.Cps`/printf, `String` turned to
  `Int => String`.
- **D. State, each path its own.** okay-zero's three: `state(0)(choose(counted))` = `(6, List(1, 3, 6))`,
  threaded outside the choice; `choose(state(0)(counted))` = `List((1,1), (2,2), (3,3))`, each path its own;
  `choose(cell(0)(counted))` = `List((1,1), (3,3), (6,6))`, one cell a run.
- **E. `Step`.** `Done`/`Yield`/`Failed`; `Async.run` driving by `Step` alone (okay-one's `std/Async`: a callback
  at once in the loop, a later one on its thread); `TestAsync`'s seven and `TestAsyncStream`'s five once `async/`
  is re-based. An exception thrown in a program comes out as `Failed`, not through the delimiters.
- **F. Stack.** `-Xss512k`: 100 000 handlers nested by recursion (`docs/stack-safety.md`), 100 000 resets and
  shifts, `flatMap` and `filter` over 100 000 empties, binds nested to the left, lifted operations in a reset.
- **G. `TestReference`** extended to `k` stored and resumed later and elsewhere, to `Step`, to `Shift % S`; a
  thousand random programs, one digest (okay-kernel's TCK does the same).
- **I. `Lower`.** printf under `choose`: `choose(reset(shift0(k => pure(x => k(x)))…))` gives a function per
  path; a `Deep` handler's `k` resumed twice past a reset — the `$` put back each time, `TestControlCps`'s programs
  with the handler outside the reset; `reset(choose(…))` does not compile.
- **J. `Raise`, `Guard`.** A `throw` in a `map` under `state` comes out `Step.Failed(e)`, the state's register
  gone with its level; `ensuring` under `attempt`: `raise` runs the finalizer, then `attempt` answers `Left`; a
  finalizer under `choose`'s `k` resumed twice runs twice; `recover` catches a `throw`, not a `raise`;
  `cancel()` at a `Yield` runs the finalizers and ends `Cancelled`, under `uncancelable` the same `Yield` is given
  back and the cancel fires at the mask's exit.
- **H. Speed**, `MonadsBenchmark`'s lanes on the same machine and JDK as `bench.d`'s last run, µs at n = 1000. The
  bar: no lane worse than okay-core-zero's by more than 5 % without a reason written in `changelog.d`; the lanes
  `k` works on at okay-eff's: answered ≤ 4.9, general ≤ 8.8, state ≤ 11.4, choose ≤ 12.9, forwarded16 ≤ 17.2; an
  operation across 16 handlers ≤ 5.8 (okay-core-zero's); captures at okay-zero's 0.31–0.46.

## 5. The landing, in order, each step a gate

| step | what | gate |
|---|---|---|
| 1 | the kernel as a package `okay.one` beside `okay.cont`/`okay.zero`: monad, rows, handlers, machine (`$` a level of `Meta`, `Lower`, `Raise`), `Step` with `Failed`, `TestReference`; std under its own names, `Shift % S` among them | okay-one's suites green here; §4 A–D, F–I |
| 2 | `okay.core` re-written over it: `Rows` (the facade) over `In`/`Removed`; `RowEffects` over `A ! R`; `std` by its names; `Stream`; `pstate` over `Control[R]` | `TestGuide` (8), `TestFacade` (5), `TestEffects` (27), `TestControl` (6), `TestControlCps` (6), `TestPState` (4), `TestStream` (13), `TestIntercept` (8), `TestNesting` (3), `TestTogether` (5), `TestDynamic` (6), `TestAnywhere` (8) — each re-written where it spoke the context language, none weakened |
| 3 | `okay.cont`, `okay.zero`, `Backend`/`Cps0`/`Spec`, `TestMonads`, `TestAgree`, the two kernels' modules deleted; `MonadsBenchmark` over the one monad; `architecture.md` re-written, `cont0-vs-cont.md` to `changelog.d` | `sbt coreJVM/test` green; `bench save`, the table in the README beside okay-core-zero's last |
| 4 | `Guard` (`ensuring`, `recover`, `uncancelable`), `Step.Yield.cancel()`; `okay.core.kernel.OnCore` through `Step`: one bridge; okay-kernel's TCK | §4 J; `kernel/` (14), `TestNative` (6), TCK; `KernelMixedBenchmark` not worse than native's 231/255 |
| 5 | `async/` over `Step` | `TestAsync` (7), `TestAsyncStream` (5), `TestAsyncCross` (4), `AsyncBenchmark` |
| 6 | `interop/cats`, `interop/tofu` over `RowEffects` | `TestCatsProgram`, `TestTofu` (6) |
| 7 | JS, Native (`crossProject` back on), `scala2/` through the TASTy reader | `sbt test` as today: every suite on every platform; `TestOwnHandlerFromScala2`, `TestTofuFromScala2` |

Steps 1–3 are the architecture; 4–7 its platforms and doors, in the order their suites depend on it. Nothing of 4–7
is touched before 3 is green.

## 6. Deferred, named

- **A handler between a `shift0` and its `$` in `Control[R]`** (okay-free's `Linear`, a `shift0` crossing a
  `Deep` handler): refused by the types, for good — the record says what it costs. What was wanted of it is had
  the other way round: a `Deep` handler's `k` crossing a `$` (`Lower`), and `Shift % S` for control under
  handlers with the answer fixed.
- **`Needs`/`Place`'s object a run**: measured in step 2 (`stateRows` against the kernel's own: okay-core's 4 % on
  `Cont`); if it costs more than that over the one monad, the facade is the place to pay it, not the kernel.
- **okay-free, okay-eff, okay-cont**: experiments concluded, their READMEs the record; frozen once their authors
  are done writing them up.

## 7. For okay-one's author, then — while the machine was being written apart (history: the kernel lives here since 2026-10-14)

Three things cost a line each now and a rewrite later:

1. **One `Meta`, the `$` a kind of level in it** — not `Delimited.eval` beside `Effects.eval`. A `Deep` handler's
   `k` (`Rest`) must be able to hold a `$` as a level, so a `reset` can sit under `choose`. Node: `Lower[R, S, O]
   (c: Cont[Control[R], S, O, S]) extends Cont[R, Nothing, Nothing, O]`; surface `reset(body)`; an operation of
   `R` inside the reset not answered in place by a `HandleIn` goes out past the `$`-level to its handler-level.
   Acceptance §4 I.
2. **`Raise(e)` as a node; the `try` at each call of user code** (`Bind`/`Map` frames, `value`, `next`, a clause),
   never around the `@tailrec` loop: on a throw the frames of the level are dropped, the levels unwound innermost
   first, the top answers `Step.Failed(e)`; `run` rethrows. `Step.Yield.resume` grows `fail(e)` beside `apply(x)`
   (the protocol's). `cancel()` and `Guard` come in okay-core at step 4 — but the unwinding they ride on is this
   one. Acceptance §4 J's first line.
3. **`Shift % S` in std** as a `Deep` effect: `reset[S]` its handler, `shift`/`shift0` its operations with `k`;
   `shift(identity)` = `pure(k)`. No node for it. Acceptance §4 B.

Who does what: 1–3 in okay-one, by its author, now; `Guard`/`cancel` in okay-core at step 4 by whoever holds the
sprint item `one-monad` then; the facade and `RowEffects` at step 2 likewise. Read §4 before writing a test, §5
before starting a step.

## 8. Where the landing stands (merged to master 2026-10-14; the branch `one-monad` goes on)

Steps 1–3, 5, 6 and 7's Scala 2 are done on the branch (changelog.d/one-monad-*.md): `okay.one` imported from
okay-one by `scripts/import-one.sh` until 45189cb, 2026-10-14 — the kernel lives here since; `okay.core` over it — `A ! E1 + E2` by `Needs`/`Place`, `Path`
found from the place first, `Op`, the handler sugar, control's two faces, `RowEffects[Eff]`; one std; `OnCore` the
kernel's `one/` bridge; the suites re-written (JVM 208, kernel 30, async 19, cats 2, tofu 9, Scala 2 6); the guide,
the docs. Not passed: §4 H — captures 2×, generate +40%, the mixed program +70% against
okay-core-zero's last (forwarded16 met since okay-one 0ff6e5e: 5.7), the kernel's items (changelog.d/one-monad-step2-3.md).

**The mixed program's regression, pinned (2026-10-14).** `MixedBenchmark.orders` (state), the facade at 2ef1dd5 on
each kernel, one machine, back to back: okay-one c7af924 158.8 µs, ee7812a 173, cec1b6a 221.5, 0ff6e5e 218.6,
feb703d 218.6; the facade at HEAD on c95137d 227–232. So the step is cec1b6a (a `k` failed or cancelled: `Fail`/`FailC`
nodes, `fails` on every `Rest`, `guardsTo` walked at every abort, the `none` evidence in `clausing`); the look-up kept
on the path (0ff6e5e) and the reset's view (feb703d) cost nothing here; 2742dfe's inline `flatMap` (`Lifts.down` and
`<:<` plumbing at every bind of user code) the last +5%. Allocation moved +3% only (1.39 → 1.43 MB a run): code
shape, not objects. By `-XX:+LogCompilation`, C2's inline budget for `eval` (its own 4420 bytes plus callees, 8000)
runs out earlier on c95137d: the second `clausing` site (`InjectHere` under `Meta.Under` — the road of every `Deep`
handler, which the facade's all are: `handleIn`) and after it `Meta.Under`'s accessors, `Cont.Under.apply`,
`InjectHere.op`/`g`, `lookUp` stay out of line; on c7af924 both `clause` sites were inlined. `-XX:-ClipInlining`
does not mend it (240), so the cliff is not the whole account. okay-bench's `OneMixed` on the kernel's own std
(`Aborting`, `Answering`) is flat across these commits — it does not take the `Deep` road. A look-up that allocates
no `Found` on a miss was tried and dropped: no change on mixed, forwarded16 worse. The fix is the kernel's
(okay-one), on the `Deep` road of cec1b6a; re-import when it lands. Deferred: `PState` (okay-one's, in
control, to be imported), Scala.js tests (the bridge does not link on master either), `Removed.Again` — an
interceptor's relocation in the kernel, so `Intercept.at` (tofu's `local`, `handleWith` on an effect anywhere in a
row) runs in the machine, not by stepping.
