## effects-once - an effect written once, its Program derived

`Program.handled(p)(h)`: a handler written in context — given a context and its body — as a handler of programs,
its effect taken off wherever it is in `p`'s row (`Removed`, `View`); `Program.within(fa)(h)` the same for an
interceptor, `fa` built again in its context. `Programs.reader`, `state`, `writer`, `throws`, `local`, `handleWith`
one line each now; one's own effect written in context gets its `Program` handler in one line too (`TestAnywhereOn`:
a `Log` handled wherever it is). Read by Scala 2 as before.

Measured (µs a thousand, Cont / Cont0): `state` as a `Program` 24.0 / 23.0 → 25.6 / 24.0, the state third in the row
38.2 / 38.1 → 39.4 / 39.0, 343 / 488 B — within a run's spread, the derivation one closure a run.
