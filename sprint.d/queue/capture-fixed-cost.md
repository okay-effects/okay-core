# capture-fixed-cost

One shift0 under a reset, `k` once (`captureOnce`): 0.117 µs and 816 B against 0.04 on okay-core-zero (×3); the
kernel's own lane the same (0.105), so the cost is in the machine, not the facade. `captureShallow` (k 8 times, one
map) 0.35 against 0.16, `capture` (16 maps) 0.90 against 0.49. What a run of one `$` and one shift0 allocates, object
by object (the entry: `Eff`, `Frame.End`, `Meta.Top`; the `$`: `Meta.Reset`/`Dollar`; the shift0: `Rest`, the
clause's program, `Resume`), and each one not needed taken out. The measure: `captureOnce`, `captureShallow`,
`capture`, their B/op; the rest of the lanes unmoved.

Measured 2026-10-14 (async-profiler on the kernel's `captureOnce`, JDK 26): not the allocation — 816 B against
okay-core-zero's 568 (+44%) while the time is ×3. No hot spot: `eval` itself 21%, the rest spread over the nodes'
and levels' constructors at 1–5% each (`Return`, `Frame.End`, `Out.None`, `Meta.Top`, `HandleK`, `Meta.Under`); a
third of the samples land on `boxToInteger` in the benchmark's own `shots` (cached small ints: the sampler's skew,
not a cost). The path is some thirteen steps of the loop for one `shift`: `reset`'s level entered, the clause
given `Rest.Under`, the body under a second `reset` (`handle(this)`, a `HandleK` and its level), each `k(x)` a
`View` over `Removed.Absent` (its level, its bottom) around a `Resume` (the third level), then each level's `ret`
back out. `shift0` (`Up0`) skips the second reset and the views: the kernel's `capture0Shallow` 0.25 against
`captureShallow` 0.37 µs; the machine's own `$` and shift0 (`captureControl*`) 10–25% under the effect's. okay-core-zero
ran `Control` in a loop of its own. So: per-node trims (a shared `Out.None`, fewer `Frame.End`s) are worth a few
per cent at most and cost `eval`'s room; the ×3 is the design of `shift` as a `Deep` effect. To try, larger: a
`View` with `Removed.Absent` around a `Resume` resumed as one level (no `UnderView` for a `k` re-entered under the
same handler kind), and `shift` built on the machine's `$`/shift0 where the row allows it.
