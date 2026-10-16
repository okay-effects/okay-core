# shift-here — the delimiter on the operation's own path

A profile of master showed the delimiter of `shift-thin` missed on the common path: a shift at the row's head
(`PerformHere`/`InjectHere`, the kernel's `shift` and the facade's `Op`) is answered in `eval` by `clausing`, so its
clause ran as a `Deep`'s — a closure, then a `View` and a `Resume` at each `k(x)`. Now `clause` knows a
`Delimiting`: its body under a `HandleK` of itself (eval's own path, its bytes as they were), its `k` a `Viewed`, each
`k(x)` ONE `Resumed` node, matched in eval beside `View` (`resuming`, out of line).

Tried and not kept: the test in eval's operation cases (`taken` there) — +20–30% on `capture` from eval's shape alone
(the same with the delimiter's path switched off); `Resumed` in `rare` — +12% on `capture`.

Measured, JDK 26, µs/op, master → this. Single lanes, interleaved twice: kernel captureShallow 0.293 → 0.245,
captureOnce 0.081 → 0.077, capture 0.658 → 0.674, captureDeep 7.41 → 7.53; facade captureShallow 0.278 → 0.250,
captureOnce 0.097 → 0.089, capture 0.70 → 0.78, captureDeep 7.37 → 7.71; queens, choose, generate, orders the same.
With `-wm BULK` (each fork warms every lane of the set, eval compiled on a mixed profile): kernel captureShallow
0.393 → 0.294, capture 0.873 → 0.881; facade captureShallow 0.399 → 0.325, captureDeep 9.29 → 8.24, capture 0.82 →
0.90 (master's own runs 0.72–0.92). With `-XX:-TieredCompilation` facade capture 4.73 → 1.69. The single-lane loss
is C2's specialization of eval to the lane (the map wrapper `Cont.map`'s lambda left a call in it), not the work:
fewer allocations, no deopts.
