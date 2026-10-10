# okay-interface — okay-core as an instance of okay's `Effects[M]`

- `okay.core.RowEffects[M[_ <: Row, _]]`: okay's root interface, its signatures okay's (`pure`, `perform` by a path,
  `defer`, `tailcall`, `flatMap`, `map`, `handle` with `Removed`, `run`), okay's words its members (`A ! R`,
  `effect(op)` and `op.perform` — an `Op` whose row is the one it is bound into — `p.handle(h)`, `p.value`);
  `RowEffects.+`, okay's `Pure + Ask + Say`. The instance `RowEffects.on(top)` over `Program`, on any monad;
  exported by the facade. Moved from TestAnywhere's prototype.
- `Program.Answering`: a tail-resumptive handler of programs, run as the monads' `Answering` — answered in place.
- okay's TestEffectsRows ported as written there (its three tests, the words included), on `Cont`, `Cont0`, `Ref`,
  `Cps0`. docs/okay.md: the interface, its handlers, what differs.

Measured, JDK 26, `-f 2 -wi 5 -i 5`: `stateRows` (the state loop over the interface) against `stateProgram` (over
`Program`): cont 23.93 / 23.88 µs, zero 23.89 / 22.98 µs — the interface costs nothing on `Cont`, 4% on `Cont0`.
