# capture-fixed-cost

One shift0 under a reset, `k` once (`captureOnce`): 0.117 µs and 816 B against 0.04 on okay-core-zero (×3); the
kernel's own lane the same (0.105), so the cost is in the machine, not the facade. `captureShallow` (k 8 times, one
map) 0.35 against 0.16, `capture` (16 maps) 0.90 against 0.49. What a run of one `$` and one shift0 allocates, object
by object (the entry: `Eff`, `Frame.End`, `Meta.Top`; the `$`: `Meta.Reset`/`Dollar`; the shift0: `Rest`, the
clause's program, `Resume`), and each one not needed taken out. The measure: `captureOnce`, `captureShallow`,
`capture`, their B/op; the rest of the lanes unmoved.
