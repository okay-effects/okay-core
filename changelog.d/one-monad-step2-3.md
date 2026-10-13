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
- The lanes (bench.d/2026-10-13T19-45_one-monad-5486b17.csv) against okay-core-zero's last, the bar of §4 H: at or
  under it on pure, answered, forwarded (1), state, stateBang, reads, readsLocal, writer, throwing, choose,
  chooseNested, collect, leftBinds, tailcallDelay; over it — forwarded16 17.5 (5.8), forwarded4 6.97 (5.54),
  general 8.79 (8.20), captures ×2, generate 21.7 (14.9), queens 159 (148), the mixed program 162 (130), the kernel's
  419 (231). The gate of step 3 is not passed by these; they are the kernel's (okay-one's §4 H work, in progress
  there), measured here as the landing will measure them.
- Then: `Path` a value class (no object an operation found from the place), `choose` the kernel's `Choose.all` (a
  list, each path's values in front of those gathered) — choose 16.0 → 13.05, chooseNested 23.0 → 18.1, the kernel's
  own 13.08. The facade's cost on a program needing effects, the kernel's own lanes beside: reads 4.3 against 3.1
  (a path found among the place's effects and checked at run time, an operation), stateBang 12.2 against state 11.1;
  the rest within noise. A cell under the mixed program slower than a state (217 against 159): the kernel's, to look at.
- okay-one re-imported at 6c83952 (Guard — ensuring, recover, cancel at a step; a look-up kept on the path; k
  failed/cancelled; captures in control): its suites 77 here, the JVM's 219. `pstate` back: the kernel's `PState` in
  control, lowered to a program of effects (`okay.std.pstate`); `TestPState` 4 — §4 C.
- The lanes on okay-one 6c83952 (bench.d/2026-10-13T23-00_one-monad-0500906.csv): forwarded16 17.5 → 5.55,
  forwarded4 6.97 → 5.34 (the look-up kept on the path — the bar's 5.8 met); the rest as before, but the mixed program
  162 → 218 (a state; a cell 222) and queens 159 → 179, generateTaken 15.5 → 17.6: a regression of the kernel's between
  c7af924 and 6c83952 (Guard, k failed/cancelled, the look-up), the kernel's to find.
