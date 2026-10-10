## translate - Effects.translate: a handler giving each operation a program outside it; c.Intercepted, an interceptor's body

- `Effects.translate[E](f)(body)`: a handler of `E` whose every clause is a program outside it, `f(op)` (a
  `[X] => E[X] => c.Prog[X]`), the rest resumed with its value; the body's value the answer. An effect of one's own
  on another is a match now, no `Handler`, `ret` or `k` written: outside (`Audit` to a `Writer`) or installed
  inside and hidden (`Ticket` on a private `State % Issued`). Its effect invariant: of a covariant one a match knows
  the value's type only from below. A clause needing `k` itself stays a `Handler`.
- `c.Intercepted[Ans, A]` = `c.Reset[Ans] ?=> c.Within[Ans, Ans, A]`, as `c.Body` is a handler's: `local`,
  `handleWith`, `Program.within`, `Effects.intercept` written with it.
- docs/guide.md's *Effects from effects* on both, operations by `effect(op)`; TestEffectsOn: translate to a writer,
  on a hidden state (the caller's untouched), 100 000 operations in constant stack — on Cont, Cps0, Cont0, Spec.
