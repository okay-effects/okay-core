# okay-words - okay's program code compiles on `import okay.*`

okay's code is mostly written in effects: `Int ! State % Int`, `A ! Choose + F`
(okay.freer's union, `F` the rest of the program's effects). The facade speaks
the same: what is missing is okay's kernel words over it.

- `effect(op)` and `op.perform`: an operation of one's own effect as a program;
- `A ! E + F` with `F` abstract — "and whatever else": a test that okay's
  polymorphic signatures (okay-agent's `samples[A, F[+_]](gen: A ! Choose + F)`)
  compile and run with `F` any effect;
- docs/okay.md: the table of okay's words against okay-core's — what is the same,
  what is renamed (`State.set` / `put`, runners), what has no counterpart.

Not here: okay's std handlers by name (`runChoice`, …) — they are okay's library.
