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
- Measured (`MonadsBenchmark.generate`, 1000 elements summed in a loop): through `Stream` 16.6 µs on Cont, 15.2 on
  Cont0, against 14.5 and 14.1 with a consumer type of the monad's own — +7–14%, +24 B an element: the closure over
  the top's run. The full run at 178f6da shows +50–78%: its consumer built a `List` (`take(n).sum`), not the stream.
- Then `Stream` one object an element: `Next` an abstract class, `w` and `rest()`; `stream` makes one that keeps the
  rest and its top, no closure. `generate` 14.6 µs on Cont, 13.8 on Cont0, 224 and 208 B an element — as the monads'
  own consumer types (14.5, 14.1; 224, 208 B): one type for every monad at no cost.
- Then `Stream` an enum: `Next(w, top: Top, rest: top.M[…])` or `Done()` — a dependent case, the rest's type its
  top's; `pull()` runs it. The same cost: 14.8 µs on Cont, 13.8 on Cont0, 224 and 208 B an element. A pattern's
  variables lose the dependence: `n @ Next(w, _, _)` then `n.top.run(n.rest)`, or `n.pull()`.
- `Stream.iterator` (the collections' `map`, `filter`, `sum`; none pulled past the last asked) and `foreach`.
  `okay.std.Gen[M, W]` out of okay.std into the tests that use it: `generate` takes any consumer type.
- `Stream.map`, `flatMap`, `filter` (for-comprehensions), `++`, `Stream.of`: a third case, `Later(w, rest: () =>
  Stream[W])`, the rest of a made stream a function — a generator's own elements stay `Next`, no closure. Lazy, the
  empties skipped in a loop: 100 000 of them in constant stack.
- Then `Source[W]`, what `stream` gives: a generator not started, a value — each consumption a run from the
  beginning, a `state` in it a cell of its own. `map` and `filter` fused into the generator's handler; pulled
  (`take`, `iterator`, `start` a `Stream`) or pushed (`foreach`, `fold`, `toList`: answered in place, nothing
  captured). A body's own handlers in the stream, pinned on the four monads: a reader; a state one cell a start.
- Measured, ns an element (Cont / Cont0): pulled 14.9 / 14.5; mapped by `Later` 21.8 / 19.5, fused 18.0 / 16.2 (the
  rest of the +24 B the boxed mapped `Long`); pushed 10.3 / 7.6.
- Asynchronous streams: a body of `Emit` and `Async` together. `Async.stream(top)(body)(on)` hands each element
  with `more()`, the demand for the next — a `yield_` an `await` of it, so the body never runs ahead of its consumer;
  a consumer asking in place a loop (100 000 elements in constant stack). `Async.iterator` the same, blocking.
  `TestAsyncStreamOn` on Cont, Cont0, Ref and Cps0.
