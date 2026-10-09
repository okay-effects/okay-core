# okay-core documentation

- [Guide](guide.md) — writing programs with effects and delimited control, the standard effects, your own effects and
  handlers, `shift`/`reset`/`shift0`, running on `Cont` or `Cont0`. Every example is compiled and run by
  `TestGuide`.
- [What `Cont0` does better than `Cont`](cont0-vs-cont.md) — the foundation, the guarantees, the semantics of a
  `shift` under a handler, the measurements; and where `Cont` is better.
- [Architecture](architecture.md) — the interface, the two monads (`Cont`; `Cont0` and its machine), the references,
  the tests and the benchmarks.

The history of the design — what was done, measured, and dropped — is in [`changelog.d`](../changelog.d); the
benchmark runs in [`bench.d`](../bench.d).
