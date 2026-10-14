# one-monad

okay-core on one monad: the kernel written in okay-one (okay-cont's `Cont[R, I, O, A]`, okay-eff's machine with
`k` as data) landed here in the order of docs/one.md §5 — step 1 the package `okay.one` with its suites, step 2
`okay.core` re-written over it (the facade, `RowEffects`, std, `Stream`, `pstate`), step 3 the two monads deleted
and the benchmarks over the one. Steps 4–7 (kernel bridge through `Step`, async, interop, platforms, Scala 2) are
each their own item once 3 is green.

Gate: docs/one.md §4 (the acceptance programs) and §5 (the suites, by step). Not before okay-one's kernel passes its
own `TestReference` with `k` as data and the `MonadsBenchmark` lanes at §4 H.
