# A run entered with no state object

`Machine.run` and `stepped` enter the loop by `eval` itself, and a step's `resume`/`fail`/`cancel` do the same: no
`Eff` made to hand the first step to `drive` (24 B a run). A generator pulled is a run an element (`Step.pull`), so
the run's fixed cost is paid per element: `generate` 20.7–22.4 → 19.0–19.3 µs, `generateMapped` 24.7–26.0 → 21.6,
`captureShallow` 0.35 → 0.34; the mixed program, `pure`, `state`, `tailcallDelay` flat (A/B twice interleaved,
JDK 26).

Tried and dropped (sprint.d generator-levels): a top of its own for a run (`Meta.Run`, no `Out`, no cancel flag;
24 B less) — `generate` +8%, `captureShallow` +8%, the mixed program +1.5%: one more kind of level in the
machine's matches costs more than the object. A single `Top` holding the step's function itself (`Give`, a run's
one capturing nothing): Scala 3 makes a polymorphic function literal an object at each evaluation, so nothing
saved.

Rebased on 558db82 (kernel-bridge-deep, tailRecM), A/B twice interleaved against it: `generate` 20.5–21.1 →
19.3–19.9 µs, `generateMapped` 24.3 → 21.8–22.3, the kernel's `generate` 23.0 → 19.7–20.1, `queens` 208–211 →
199–201; the mixed program, `pure`, `state`, `captureShallow`, `generateTaken` flat. Every suite green but
`scala2/test`: `Handler.at` (1bb647f) returns a union, which Scala 2 cannot read through TASTy — not this change.
