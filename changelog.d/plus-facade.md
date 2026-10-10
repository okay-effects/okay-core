## plus-facade - `A ! E1 + E2`, and `import okay.*`

- `+` joins effects: `Reader % Int + Writer % String + Async` — two effects joined an effect again (`Both`, a
  marker), so it goes on; `Needed[F]` makes the row of them by a match type, in the order written. `A ! F` is
  `(c: Needs[Needed[F]]) ?=> c.Prog[A]` — what it was: a program in any context with those effects, in any order,
  among any others; written now as the user writes it, a row no more (`A ! E1 +: E2 +: Pure` does not compile).
- `import okay.*`, the user's package: `!`, `+`, `%` (and for one's own effects `:?`, `Row`, `+:`, `Pure`), aliases;
  the standard effects, their operations and handlers, `perform`, `pure`, `reset`, `shift`, `Effects`, `Handler`,
  `Answering`, `Program`, `Top`, exported; `run(p)` on `Cont0`, `runOn(top)(p)` on another monad. Aliases, not exports,
  for the types: an export of a sealed trait crashed Scala 2's TASTy reader once. Read by Scala 2 as before.
- docs/guide.md and README written with `import okay.*` and `run`; `TestFacade`: a user's program with three effects
  joined, run on both monads.
