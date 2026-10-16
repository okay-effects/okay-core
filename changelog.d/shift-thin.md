# shift-thin — a shift's k one level, std reset on the machine's own delimiter

Toward one thin layer for delimited control (steps 1–2 of several):

1. A `View` around a `Resume` of a rest whose outermost level is a handler taking it (a shift's `k` in its body) is
   put back as ONE level, `Meta.UnderViewed` (and `Rest.UnderViewed` when it is taken again): the handler's level and
   the row's adaptation merged, no `View` level and no `Resume` step between them.
2. `handler.Delimiting[E, T, S]`, a `Deep` the machine knows: its `under(op, k)` runs a clause ON its own level as it
   stands — no `handle` around the body, no `HandleK`; its `k` a `Viewed`, each `k(x)` one `Resumed` node going
   straight in as `UnderViewed` — no closure, no `View`, no `Resume`. std `Shift.reset` is one (`shift`'s body
   under it; `shift0`'s through `apply`, as before). The delimiter's cases are off the hot loop (`rare`; `outE` the
   same as before for any other handler).

Measured, JDK 26, `-f 1 -wi 3 -i 4` interleaved twice, µs/op, before → after: KernelBenchmark capture 0.915 → 0.661,
captureDeep 9.17 → 7.34, captureShallow 0.347 → 0.291, capture0Shallow 0.243 → 0.215, captureOnce 0.085 → 0.081;
choose, generate, MixedBenchmark orders and MonadsBenchmark (queens 206.8 → 207.2, six runs each) unchanged.
