## zero-lookup-once - an operation's handler found once per path, a compare after: across 16 handlers as across 1

- `In` keeps its last lookup, `found`: the handler of `E` answering in place (or none) and the handlers in force it
  was found from. The machine's `inPlace`: the first handler's at once, as before; one further out by `found` if
  from the same handlers, else walked and kept. The same path through the same handlers finds the same, and a
  loop's operations come with one path — so the walk is once, not per operation.
- Typed by `E` in the path itself: no cast (a cache in the handlers' entry, keyed by the path, needed one — tried,
  as fast, dropped). One object, its fields final: read whole from any thread.
- Measured, Cont0, ns an operation, before / after: across 1 handler 5.50 / 5.47, 4 handlers 6.61 / 5.24,
  16 handlers 17.17 / 5.49; answered, general, state, `!`, throws, choose unchanged; allocation the same.
  Against Cont across 16 handlers: 16× (90.4).
- Also: CI (GitHub Actions, JDK 25: the tests and the benchmarks' build); `Handler`'s and `Answering`'s comments on
  their traits; Cont0 the default in README and architecture, Cont the second implementation.
