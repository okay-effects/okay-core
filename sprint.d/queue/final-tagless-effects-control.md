- [ ] final-tagless-effects-control — abstract the programs from the monad,
      final tagless, the context IS the instance (variant B). `Effects` gets
      `type M[_ <: Row, _, _, _]` beside `R` and `S`; a hierarchy `IxMonad`
      (pure, flatMap, map over `M[R, I, O, A]`, Atkey's indexed monad) →
      `Effects` (perform, handle) and `Control` (shift, reset), independent
      of each other. `Handler[M, E, T, Q, A, Ans]`. `Cont` is the instance;
      `A ! Es` becomes `c.M[c.R, c.S, c.S, A]`, polymorphic in the monad.
      A reference CPS instance of `Control` — functions `(A => I) => O`, over
      `Pure` — for the tests only: the same programs on both, results
      compared. TestCont and the benchmarks stay on the concrete `Cont`.
      Speed: calls go through the instance; with one instance at run time
      the JIT should devirtualise — measured, against the baseline.
