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
- Then one type for every monad: `Top` at any answer (`type M[J, I, O, A]`, `at[J]`, `run[J]`), and
  `Stream[W]` in okay.std, `Next(w, rest: () => Stream[W])` or `Done()`: the rest closed over its top's run, so
  the type names no monad; `stream(top)(body)` makes one, `take(n)` pulls in a loop. `okay.cont.Gen`
  and `okay.zero.Gen` gone; `TestStreamOn` on Cont, Cont0, Ref and Cps0.
