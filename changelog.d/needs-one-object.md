## needs-one-object - A ! Es: the context of its body an At itself, the path kept in it

- Measured where `A ! Es` cost (stateGetPutBang 17.6 against stateGetPut
  14.9 µs): EffectsBenchmark.stateGetPutHoisted — a `!` program finding its
  path once and running the plain loop with it — is 14.93, the plain
  loop's. So the cost was finding the path at every operation, not the
  context: `found → n.find(r) → paths.find(r) → head`, each step a call on
  a trait — an interface, a check of the receiver's class (tagless-zero-cost).
- `Effects.needing(paths)`: the instance makes the context of an `A ! Es`
  itself. For `Cont` that is an `At` — `NeedyOne` (one effect, its path kept
  in it) or `NeedyRow` (a row, its `Sub`) — so an operation is `At`'s own
  call, nothing forwarded, and finding a path is one call, not two.
  `Needs.Forward` stays, for an instance with no way of its own. `Paths`
  (one / a row) is a sealed abstract class with the kind-chosen case; each
  case makes its `At`, so no match on a kind is needed there.
- Two earlier tries, stopped by the language, are in the item's history: two
  context parameters in a row ("curried dependent context function type …
  not yet supported"), and the body's context found by a given in the
  implicit scope (searches for `Needs[?]` everywhere, ambiguities).
- Measured, full run kept (bench.d/2026-10-08T15-28-10_c25f976+.csv):
  stateGetPutBang 15.92 against stateGetPut 14.91 µs — from +18% to +7%,
  ~0.5 ns an operation: the check before `n.find`, `Needs` being a trait so
  it can be mixed into `At`. Every other benchmark as before (Queens' +69%
  in the full run was its neighbours: alone, 527 µs, 5 247 828 B).
