# mixed-abort-road

`MixedBenchmark.orders` (state) 221 µs since cec1b6a, 159 on okay-one c7af924 (docs/one.md §8, the account in full).
Not the facade's handlers: in this program `k` is taken by `first` (0–3 calls an order) and `throws` (`Aborting`)
only, and both need it; `ask`, `modify`, `tell` answer in place. What is left to try, each an A/B on the lane:
the abort road — an `Aborting` clause goes straight to its handler's level when no guard is between (`aborted`: `guardsTo` first, the `AbortTo` exception made only with a guard between; checked 2026-10-14), so what is left there is `guardsTo`'s walk of the levels at every abort; the levels and rests on the `Deep` road (20% of the allocation: `Meta.Under`,
`Rest.Under`, `Resume`, `HandleK`, `Frame.End`, `Defer`), a `Resume` with no `Rest` object for a `k` called once
at once; the facade's fitting (10%: `handleIn`'s closure, `Needs`, `Paths.One`, `Removed.Head`, `Req`, `In.Here`),
a `Needs` and its paths made once per handler site rather than per fitting. Tried and dropped already (§8): a
look-up with no `Found` on a miss, the loop's arms reordered. Co-ordinate with the session on one.md §8 first.

Measured 2026-10-14 (JDK 26, master 3b9d982): 220 µs, 1.43 MB a run — okay-core-zero's last allocated as much
(1.37 MB) in 151 µs, so the gap is not the objects. `eval`'s inline tree in this program (`CompileCommand
PrintInlining` on `eval` alone): its 3694 bytes plus ~4000 inlined reach `DesiredMethodLimit` (8000) at bci ~2546;
from there `lookUp` for `Perform`/`Inject` (the road of `ask`, `modify`, `tell`), `Resume`, the `Handle*` nodes and
`Defer` are calls, their accessors and `Meta.Under.<init>` out of line. The budget goes to `clausing`, inlined at
both of its sites (673 bytes each, `Choose`'s clause inside), and to the program's own lambdas at the `guarded`
sites (517, 711, 622 bytes: profile-driven, the user's code). Tried: `clausing` kept out of `eval`
(`dontinline`), A/B twice interleaved — mixed 220 → 227 / 228 µs (worse), queens 196–201 → 195–203, the rest flat:
the clause inlined is worth more than the room it frees. Not the account either, then. async-profiler on macOS
(itimer, even with `DebugNonSafepoints`) puts a fifth of the samples on a one-line lambda of the benchmark
(`_.getOrElse(0)`): no deoptimization storm (`-Xlog:deoptimization`: six, all before the measurement), a skew of
the sampler — a Linux `perf`/`cpu` profile is the tool for this, not the Mac's.
