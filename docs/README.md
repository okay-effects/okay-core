# okay-core documentation

- [Guide](guide.md) — writing programs with effects and delimited control, the standard effects, your own effects and
  handlers, `shift`/`reset`/`shift0`, on the one monad. Every example is compiled and run by
  `TestGuide`.
- [What `Cont0` did better than okay-cont's `Cont`](cont0-vs-cont.md) — before the one monad: the foundation, the
  guarantees, the semantics of a `shift` under a handler, the measurements (`Cont0` is the one monad's `Cont` now).
- [The kernel, `okay.one`](one-kernel.md) — the account of the kernel: rows, the nodes, the handler forms, the
  machine, control as an effect of the row, failure, guards, the specification, speed.
- [Control and audit of effects: interceptors](intercept.md) — a sensitive effect handled only under its audit
  (the guarantee by types), the audit's `Interceptor` and `Pass`, a plain effect intercepted, the observer of every
  effect, the trail as an effect, where an interceptor stands and what it sees, the machine's level, the cost.
- [On one monad: the landing plan](one.md) — the decision of 2026-10-13 and the order of the landing, its gates.
- [Architecture](architecture.md) — the kernel (`Cont` and its machine) and the facade over it, the references,
  the tests and the benchmarks.

The history of the design — what was done, measured, and dropped — is in [`changelog.d`](../changelog.d); the
benchmark runs in [`bench.d`](../bench.d).
