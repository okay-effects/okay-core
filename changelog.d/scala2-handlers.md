## scala2-handlers - one's own handlers from Scala 2

A `Program.Handler`'s clause takes `Program.Ops[R]` — the programs outside the handler as a type of their own, `P`,
made and bound by plain methods (`pure`, `flatMap`, `map`, `perform`) — not a context and Scala 3's extension methods,
which Scala 2 cannot see; `Program.ops(c)` a context's. `Program.HandlerAt[O, E, A, Ans]` and `Program.handleAt`: a
handler for a known row outside, its clauses performing that row's effects (`ops.perform`, by the path in `O`).

`scala2/`'s `TestOwnHandlerFromScala2`: an effect (`Counter`) and its handlers written in Scala 2 — one counting, on
Cont0 and Cont; one whose clause tells a writer outside it. An operation there gives its own answer
(`Counter.answer`): Scala 2's pattern match cannot tell an operation's `X`, so a clause answers with no cast.
