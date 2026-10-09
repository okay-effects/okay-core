## one-interface-many-monads - Effects and Control over any monad of levels; okay.core the interface alone

- A delimiter's context is a member of the context — `Handled[E, S]` (a handler's body), `Reset[S]` (a reset's),
  `Answer[S]` (a shift's) — and a handler's and a reset's body is in `Inner`, the monad ONE LEVEL IN: Cont's the
  same, Cont0's a level deeper. `handle` and `reset` take the body in its context.
- `Effects extends Control`: a handler is a delimiter answering its effect's operations, a reset one answering none;
  `Delimited` gone. `IxMonad` into `Context`. The row out of the monad: `M[I, O, A]`, its row the context's `Effs`;
  `c.Prog[A]` a program here, `c.Body[E, Ans, A]` a handler's body. One handler, `Handler[P, E, A, Ans]` /
  `Answering`, over the programs outside it; `Value` what answering in place needs. `shift` carries `same`, the
  proof it keeps the answer (`Keeps`), what a monad whose `k` holds a handler's clauses needs.
- Packages: `okay.core` the interface (rows, Context, Control, Effects, Handler, Needs, `A ! Es`, Top); `okay.cont`
  Cont, its loop and fold, the instance `At`, `run`; `okay.zero` Cont0, the machine, the instance `At`, `run`,
  `shift0`. `Paths` keep their paths in an instance's own context by `Paths.Keep` — the interface knows no instance.
- `okay.std` once, over the interface: one `generate`, of the consumer's own type (`next`, `done`); `PState` in the
  run's reset context.
- Measured: Cont's own speed unchanged (MonadsBenchmark, DispatchBenchmark).
