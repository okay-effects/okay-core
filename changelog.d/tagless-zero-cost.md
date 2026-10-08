## tagless-zero-cost - the type classes at the core's speed: abstract classes, not traits; a program's bind its own

- The cause, by experiment (DispatchBenchmark, one program five ways, one
  run): parameters, closures and `perform` through the instance cost nothing
  (contParams 55.7, atStatic 55.3 against cont 56.3 µs); the context typed
  `Effects` cost ~1 ns an operation (tagless 66.1, and monadKnown 65.8 — the
  monad fixed to `Cont`, the bind static, the same), per operation, not per
  run (n = 1 000: 5.56 / 6.81 µs; n = 100 000: 552 / 646 µs). The only
  difference left was the declared type of `c`: the final class `At`, or
  `Effects`.
- Why: a Scala `trait` is a JVM interface, and the JVM verifier does not
  check interface types — to the JIT an `Effects` is an `Object` — so C2 put
  a check of the receiver's class before each `c.perform`, one
  implementation loaded or not. A class's type is verified: class hierarchy
  analysis then calls the one implementation straight, no check. That is the
  dispatch at compile time a JIT can give: the type class is a value
  (`using c`), and a recursive program cannot be expanded by `inline`.
- So `IxMonad`, `Context`, `Effects` are abstract classes; `Control` stays a
  trait, so that an instance can have both (`Delimited extends Effects with
  Control`, which `At` is; with the F-bound below, `Effects & Control` made
  the compiler fail, an assertion in `LazyRef`, Scala 3.9.0).
- Tried on the way and taken out after (needs-one-object): a program's bind
  its own, `type M[…] <: Ix[M, …]` — it moved nothing (tagless 66 µs with it,
  under traits), and its F-bound made the compiler fail again as soon as a
  class mixed `At` with `Needs`. The bind is the instance's extension again,
  a call to an abstract class's method now, as fast. Kept: Cont.flatMap 61 →
  32 bytecode bytes (`Cont.bound`), inlined at depth; the CPS reference's
  function in a class.
- Measured, full run kept (bench.d/2026-10-08T14-56-53_0d35d1b+.csv, a quiet
  machine, errors ≤ 2%): against 49aa8d1, from before the classes, no
  benchmark slower, allocation equal in all; tailcallHandled 55.7 µs,
  tailcallHandledCont 56.7; DispatchBenchmark's five 54.7–57.3. Left:
  stateGetPutBang 17.15 against stateGetPut 15.06 µs — the cost of `A ! Es`
  itself, there before the classes (17.1 at 75a3ddf): needs-one-object.
