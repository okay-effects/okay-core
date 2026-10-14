# guard-at-answers (2026-10-14)

A guard at any answers: `Guarded` with the answers through as a `Handle`'s (`k: Frame[T, X, I, A, J]`, `body:
Cont[T, I, O, X]`, `Cont[T, J, O, B]`), `Meta.UnderGuard`/`Rest.UnderGuard` as `Through`'s; a shift0 passes a guard
as it passes a `Passing` handler (`shifted`: `never` there before), its `k` holding it — the finalizer once a
resumption. `ensure`, `uncancelable` on `Cont[R, I, O, A]`; `guard(g)` inline, the kind by the static type of `g`:
a `Recovering` guard (`Guard.recover`, the class split off `Guard`) on `!` only — what it recovers with is a program
of effects, run in the program's place, and a shift0 could not carry a "same answer" evidence past the guard (the
rest's answer is the shift0's, not the level's). The evidence `none: (J =:= Nothing) | Null` on the node and the
levels; `ensure`/`uncancelable` inline too, attaching it when the program is one of effects, so a guard's `ret` on
`!` runs as it did — no `Lift`, no level (`left`). Under a `$`: `ensure` around a shift0 resumed twice runs the
finalizer twice, a throw in the rest runs it once then fails, `uncancelable` holds its mask through the shift0
(TestGuard); `ensure` around a typestate step in okay's words (TestAtm, the guide). TestReference unchanged: errors
and finalizers are outside the meaning it checks. The lanes (bench.d/2026-10-14T08-55-55, JDK 26, against 9557506): the kernel's within ±1%, `MixedBenchmark` state 226.6 µs (+2%), `captureControl` 878 ns (0%); the facade's `MonadsBenchmark` lanes of that run disturbed by another session's build (fib +548% at ±11 µs), rerun alone back at their figures (fib 3.67 µs, forwarded 5.53, leftBinds 6.89).
