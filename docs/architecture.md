# Architecture

One monad, one machine; a facade over it. The kernel is okay-one's, imported as it is (`okay.one`); the facade
(`okay.core`, `okay.std`) is what a user writes programs in. No macros, no runtime tests of an operation's class, no
cast in the kernel; the facade has two `@unchecked` matches, named below.

```
okay.one        THE KERNEL (it lives here; it came from the okay-one repository, last at 45189cb, 2026-10-14)
  Cont         the monad: Cont[R, I, O, A] — R a row of effects (A ! R: no answers) or Control[R] (the answer moved)
  row           Row, Pure, E +: T; In[E, R] the path to an effect; Removed[E, R, T] a handler's place in a row
  handler       the forms, values: Answering, Stateful, Translating, Aborting, Deep (k: X => B ! T, data)
  machine       Machine: one CEK loop over one Meta — handlers' levels and $'s; Step
  std           Reader, State, Writer, Throws, Choose, Shift % S (control as an effect), Async (on Step)
okay.core       THE FACADE
  Rows          Eff[R, A] (the kernel's A ! R); %, +, :?, Has; Path, Req, Paths, Needs, Place; A ! E1 + E2; Under;
                Delimited, Atm[A, X, Y, Es] (the answer moved, no row named), UnderAt
  Effects       Op (an operation bound where used), Handler, Answering; handle, handleIn, translate, intercept, run
  Control       reset/shift/shift0 over Shifts[S, T]; cont/lower for Control[R]
  RowEffects    okay's Effects[M] over Eff
  kernel        OnCore: okay-kernel's Effects and Control on the one monad (docs/kernel.md)
okay.std        the effects by their names: reader (local), state, cell, writer, throws (handleWith), choose, first,
                Emit (collect, generate), Stream/stream, Step
async/, interop/, scala2/   not yet re-based on the one monad (docs/one.md §5, steps 5–7)
```

## The kernel, in one page

The whole account is [one-kernel.md](one-kernel.md); what the facade relies on:

- **A program of effects has no answer types**: `A ! R` is `Cont[R, Nothing, Nothing, A]`. That is what lets one
  program run under any handlers. Answer types move only in `Control[R]` (shift0 and `$`, Materzok–Biernacki, typed
  as Asai–Kameyama), where the effects of `R` are answered in place; a program of effects enters control by `cont`,
  a closed program of control is one of effects again by `reset` (`Lower`).
- **Rows are nominal** (`E +: T`), an operation carries its path (`In`), a handler its `Removed` — so a handler takes
  its effect off wherever it is in the row (`View`: the body seen with the effect first, each path relocated).
- **A handler is a value of one of five forms**, over the row outside it, `T`: `Answering` and `Stateful` are
  answered in place (the state a register of the machine, snapshotted into each `k` taken past it — each path of a
  choice its own); `Translating` runs a program outside for each operation; `Aborting` drops the rest; `Deep` gets
  `k`, a program over `T` — data: dropped, resumed, resumed again, kept, resumed anywhere `T` is handled.
- **The machine** is one `@tailrec eval(c, k, m)`: `c` the node, `k` the frames of its level, `m` the levels
  outside — one `Meta`, a handler's level (`Under`, `UnderState`), a view's, a `$`'s. An operation answered in place
  goes straight into the frame on top; else out past each level, each put around its rest in a `Rest`; `k(y)` a
  `Resume`, the levels put back on the `Meta` in force where it is resumed. A `Raise` is a node: user code throwing
  is caught at its call, the levels unwound, the top answers `Step.Failed`. Everything in constant stack: the levels
  are on the heap, never JVM frames.
- **`Step`**: a program run to its value, or to its first operation of the row at the top, given back with its rest
  (`Yield(op, in, resume)`), or `Failed` — what `Async`, a generator pulled, and the kernel protocol's `Computation`
  are built on.
- **The specification** is `TestReference`: every node's meaning in CPS, random programs' answers pinned by a digest.

## The facade

Four requirements, one design: a small kernel people can read and use on its own; the kernel of okay, whose
programs are `M[R, A]` over `Effects[M]` and whose handlers take an effect off the row wherever it is; Scala 2.13
from the same artifact (nominal rows, no inline, no opaque types in what it reads — step 7); code written over
`F[_]` — cats, tofu (step 6).

- **`A ! E1 + E2`** — a program of value `A` needing the effects `E1`, `E2`: `(c: Needs[Es]) ?=> Eff[c.R, A]`, a
  function of the place it runs at. The place says the row (`Place[R]`: a handler's body, `E` in front of the row
  outside; the top; one named by hand, `Place.at`); `Needs` finds the paths of the effects in it (`Paths`, by
  `Req`: the one effect, or the left or right of a `+`), one object a fitting. The handlers make the row, outside
  in; a program names only what it needs. `+` joins effects into one marker (`Both`); an effect not yet known is
  one like any, `A ! Choose + F` — `F` inferred at the call. (`Paths.One.find` and `Paths.Both.find` match
  `@unchecked`: `Only` is one effect's case and `Left`/`Right` two joined's, which the checker cannot tell apart; the
  givens choose by priority.)
- **`Path[E, R]`**, the facade's evidence that `E` is in `R` — found FROM THE PLACE FIRST: in a program needing
  effects, from its `Needs`, whatever row `R` is still unknown to be (`Path.fromNeeds`); else by the row, the
  kernel's path walked (`here`, `there`), or the kernel's `In` given by hand. So an operation bound first in a chain,
  `get.flatMap(f).flatMap(g)`, is of the program's row, not of a row the compiler would otherwise make up for it from
  `In.here` with the row unconstrained. `E :? R` and `Has[E]` are `Path`. A value class: no object an operation.
- **`Op[E, X]`**, an operation not yet a program: `perform(op)`, `ask[Int]`, `tell(w)` — bound where it is used
  (`flatMap`, `map`, a conversion where a program is expected), the row the expected type's; or named, `at[R]`.
- **Handlers.** `Handler[E, A, Ans]` (`ret` a value, `apply(op, k)` over any row outside) and `Answering` are sugar
  over the kernel's `Deep` and `Answering` (`at[T]`); a handler made of the effects outside is the kernel's `Deep`
  over a row that has them, by `handleIn`. `handle(h)(body)` puts the handler at the head of the row outside, the
  body `Under[E, T, A]` — a context function of its place, built when the run enters the handler (`defer`), so a
  handler installed by a recursive call costs no stack. `translate` (the kernel's `Translating`), `intercept` (a
  handler of an effect already in the row, its clauses' own operations outward: `local`, `handleWith`).
- **Control, two faces** (`okay.core.Control`): `reset[S, T](body)`, `shift`, `shift0` over `Shifts[S, T]` — the
  kernel's `Shift % S`, control as an effect of the row with the answer fixed, under any handlers in any order; a
  shift directly under a handler holds the handler in its `k`. `cont` and `lower` for `Control[R]`, the answer
  moved — printf, `PState`.
- **okay's interface**, `RowEffects[Eff]`: okay's `Effects[M]` over the kernel's program itself, nothing built under
  it ([okay.md](okay.md)).
- **The kernel bridge**, `OnCore`: okay-kernel's protocol as a library over `Deep`, nothing in the machine
  ([kernel.md](kernel.md)).

## Tests

`src/test/scala/okay/one`: okay-one's suites as imported — `TestReference` (the meaning), `TestGuarantees` (what
the types refuse), `TestCont`, `TestControl`, `TestShift`, `TestStd`, `TestStep`, `TestRaise`, `TestData`.
`src/test/scala/okay/core`: the facade's — `TestEffects` (the standard effects, `translate`, generators),
`TestContext` (`A ! E1 + E2`, control as an effect), `TestControl` (both faces), `TestDynamic` (a functional state by
hand, two handlers of one effect by path, 500 handlers crossed), `TestIntercept`, `TestNesting` (100 000 handlers
nested in constant stack), `TestTogether`, `TestStream`, `TestAnywhere` (okay's `TestEffectsRows` over
`RowEffects[Eff]`). `words/src/test`: `TestFacade` (`import okay.*` alone); `TestGuide` and `TestGuideDoc` (the guide's
text compiled: its `scala` blocks generated into `GuideDoc` by `project/GuideDoc.scala`, each `expr   // value` line
checked).
`TestNative` and `kernel/` (the bridge, okay-kernel's TCK).

## Benchmarks

`sbt "bench -prof gc"` runs them and prints a table; `sbt "bench save -prof gc"` keeps the run in `bench.d`.

- `MonadsBenchmark` — the lanes: binds, maps, tail calls, operations answered in place and by a clause, across 1,
  4 and 16 handlers, the standard effects, `A ! Es`, generators, 8 queens, captures under a reset.
- `MixedBenchmark` — a program of several effects, 1000 orders; `KernelMixedBenchmark` — the same over the kernel
  protocol, on `OnCore` and on okay-kernel's reference machine.

The lanes beside okay-core-zero's last, and what is behind: the README; the bar, docs/one.md §4 H.
