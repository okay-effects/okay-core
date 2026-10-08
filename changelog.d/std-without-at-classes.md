## std-without-at-classes - okay.std without its helper classes; one name for a path, one spelling of a row

- The ten classes that only split a call's type parameters — ReaderAt,
  StateAt, WriterAt, ThrowsAt, ChooseAt, CollectAt, GenerateAt, PStateFrom,
  ResetAt, ShiftAt — are gone: clause interleaving,
  `def state[A](using c: Effects)[S](s0: S)(body)`. Calls read as before
  (`state[Int](0)(body)`, `throws[String, Int](body)`, `state(0)(body)`);
  a context passed by hand now goes right after the types,
  `state[Int](using At[Pure, (Int, Int)]())(0)(body)`.
- `Has` is gone, `In[E, R]` is the one name for "E is in the row R".
- `+` is gone, a row is spelled `E +: R` only; its test went with it
  (68 tests, all green).
- Measured (full `bench -prof gc` against 64c2929): allocation unchanged
  everywhere. Queens' +126% in the full run was its neighbours: alone, 517 µs
  and 5 246 332 B, the baseline's bytes exactly, and 522 µs before the change.
