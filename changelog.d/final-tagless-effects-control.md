## final-tagless-effects-control - Effects and Control, type classes over an indexed monad; the context is the instance

- `IxMonad` (`type M[R <: Row, I, O, A]`, `pure`, `delay`, `flatMap` and
  `map` as extensions), `Context` (`R`, `S`), and two classes over it,
  independent: `Effects` (`perform`, `handle`, `under` — the context at
  another row and answer, the same monad) and `Control` (`shift`, `reset`,
  raw; `Control.shift` / `Control.reset` in context). `Handler[M, …]` and
  `Answering[M, …]`.
- The context IS the instance (variant B): `(using c: Effects)` and a program
  is a `c.M[c.R, c.S, c.S, A]`; `A ! Es` is `c.M[…]` too, polymorphic in the
  monad. `Effects.At` is the instance over `Cont`, effects and control both;
  `Effects.run` runs on it.
- okay.std written against the classes: every operation and handler on
  `c.M`; `Gen[M, W, R]`; `PState[C <: Control & Singleton, R, Q, V]` over
  any `Control` (a test passes one context by hand to `pstate` and the
  body); `Async.run` on `c.delay`, `runAsync` stays on `Cont` (it is a
  runner). Benchmarks' programs go through the classes as users' do.
- `Needs` (the context of `A ! Es`) is made of the context it stands in and
  forwards every operation to it.
- TestControlCps: a reference `Control` — functions `(A => I) => O`, the
  textbook CPS — and six programs written against `Control` (shift/reset,
  answer type modified, multi-shot, nested resets, a shift at the top, and
  `pstate`) run on `Cont` and on it, both checked. 75 tests, all green.
- Measured: allocation unchanged in all 34 benchmarks. The full run was
  noisy (errors up to ±110 µs), so the suspects were run base (75a3ddf) /
  HEAD back to back: tailcallPure 35.3 / 35.2 µs, tailcallHandled 63.6 ±19 /
  67.1 ±0.2, tailcallHandledGeneral 93.0 / 89.6, tailcallForwarded 122.9 /
  118.7, tailcallDelay 26.7 / 23.1, stateGetPut 15.61 / 15.46, captureDepth
  equal. One loss: stateGetPutBang 17.7 → 20.6 µs (+16%), the forwarding of
  each operation through `Needs` — needs-one-object's to win back.
