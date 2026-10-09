## top-generators-docs - c.top, Gen on each monad, the tests by package, the docs

- `Effects.top`: every context has its monad's `Top`. `Async.run(body)(using c: Effects, …)` runs on `c.top` — no
  given `Top` to name at the call.
- `okay.cont.Gen[W]`, `okay.zero.Gen[W]`: a generator's consumer at the top, `Next(w, rest)` / `Done()`, with
  `take(n)`; `generate` on either monad needs no type of one's own. `MonadsBenchmark.generate` uses them.
- `TestCont` in `okay.cont`, beside the monad it tests.
- docs: `docs/guide.md` (every example compiled and run by `TestGuide` on both monads), `docs/architecture.md`,
  `docs/cont0-vs-cont.md`; README the front page.
- Known: Scaladoc warns with a ClassCastException on `Async.run`'s signature (a dependent context function over a
  refinement) — a toolchain bug; the docs build, the page for it lacks the signature.
- Measured: `MonadsBenchmark.queens` a loop, not `zipWithIndex` — the pairs, made or not as the JIT inlined it, were
  most of the run and its spread.
