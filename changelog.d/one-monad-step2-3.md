## one-monad, steps 2 and 3 - okay.core over okay.one: the facade re-written on the one monad, the two monads gone

- `okay.core`, the facade, over `okay.one`'s program `Eff[R, A]` (its `A ! R`, the row first): `A ! E1 + E2` is
  `(c: Needs[Es]) ?=> Eff[c.R, A]` — a program fitted to the place it runs at, `Needs` and `Place` the facade's
  (docs/one.md §3); `Path[E, R]`, the facade's evidence, found from the place first (`Needs`), then by the row — so an
  operation bound first in a chain is of the program's row; `Op`, an operation bound where it is used; `Handler`
  (`ret` a value) and `Answering` as sugar over the kernel's `Deep`/`Answering`; `handle`, `handleIn` (the kernel's
  forms over the row outside, made of its effects), `translate`, `intercept`, `run`. The context language — `Context`,
  `Effects`, `Control`, `Top`, `At`, `View`, `Program` — gone.
- `okay.std` by its names over the kernel's std effects (one std: `Reader`, `State`, `Writer`, `Throws`, `Choose` are
  `okay.one.std`'s, re-exported): `reader`, `local`, `state` (a register, each path its own), `cell` (one a run),
  `writer`, `throws`, `handleWith`, `choose`, `first`, `fail`; `Emit` here: `collect`, `generate`, `Stream`/`stream`
  (`k` as data its rest), `Step.Next(w, rest)`. `modify` a program needing the state.
- Control, two faces (`okay.core.Control`): `reset`/`shift`/`shift0` over `Shifts[S, T]`, the kernel's `Shift % S` —
  control as an effect of the row, the answer fixed, under any handlers; `cont`/`lower` for `Control[R]`, the answer
  moved (printf in `TestControl`). `PState` not here yet: okay-one is writing it in control (c7af924).
- `okay.core.kernel.OnCore`: okay-kernel's `Effects` and `Control` on the one monad — okay-kernel's `one/` (OnOne) as
  it is; `kernel/` 30 green (its TCK 16).
- Deleted: `okay.cont`, `okay.zero`, their kernels, `Backend`/`Cps0`/`Spec`, `TestMonads`, `TestAgree`,
  `TestControlCps`, `TestPState` (back with `PState`), `DispatchBenchmark`, `ZeroShift0Benchmark`.
- The suites re-written where they spoke the context language, none weakened: JVM 208 (TestEffects 27, TestContext,
  TestControl, TestDynamic, TestIntercept, TestNesting — 100 000 handlers nested in constant stack, no pin —
  TestTogether, TestStream, TestAnywhere over `RowEffects[Eff]`, TestFacade, TestGuide, TestNative) and okay-one's 66;
  docs/guide.md re-written, its blocks compiled (`TestGuideDoc`).
- `MonadsBenchmark`'s lanes and `MixedBenchmark` over the one monad; `KernelMixedBenchmark` on `OnCore` and `steps`.
- Not re-based yet (docs/one.md §5, steps 5–7): `async/`, `interop/`, `scala2/`. Scala.js: the core compiles; the
  test bridge fails to link on master too (`CompletionLatch.releaseShared`), a toolchain matter, step 7's.
