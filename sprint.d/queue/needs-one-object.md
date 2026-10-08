- [ ] needs-one-object — `A ! Es` costs against plain givens:
      stateGetPutBang 17.9 ns against stateGetPut 15.7 ns (64c2929). A call
      of a `!` program makes a `Needs` and its `Paths`, an operation a `Req`.
      One object, the paths found once; goal: Bang level with the givens.
      Since final-tagless-effects-control (2026-10-08) `Needs` forwards every
      operation to the context it is made of: stateGetPutBang 20.6 µs against
      stateGetPut 15.5 µs, back to back.
      Tried 2026-10-08, both stopped by the language, reverted:
      (1) `A ! Es = (c: Effects) ?=> Needs[Es, c.R] ?=> c.M[…]`, operations
      straight to `c`, `Needs` only paths: "Implementation restriction: …
      is a curried dependent context function type. Such types are not yet
      supported." (2) `Needs[Es] { val c: Effects }`, `A ! Es = (n: Needs[Es])
      ?=> n.c.M[…]`, the body's context `n.c` by a `given current` in object
      Effects: that given sits in the implicit scope of every search for a
      context or a path, so open searches for `Needs[?]` start everywhere and
      their ambiguities (Sub.last/cons, Needs.one/row) fail searches that
      have a path at hand; priorities quieted two, the third remained.
      Where it stands (49224cc): stateGetPutBang 18.0–18.5 µs against 17.1
      at 75a3ddf. Left: a cheaper forward (no bridges), or accept.
