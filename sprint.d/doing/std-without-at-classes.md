- [ ] std-without-at-classes — mechanical simplifications, no speed change
      expected. (1) The `*At` helper classes (ReaderAt, StateAt, WriterAt,
      ThrowsAt, ChooseAt, CollectAt, GenerateAt, PStateFrom, ResetAt,
      ShiftAt) go: clause interleaving, `def state[A](using c: Effects)[S]
      (s0: S)(body)` — checked to compile and infer, `state[Int](1)(…)`,
      `throws[String, Int](…)`, `state(1)(get[Int])`. (2) One name for "E is
      in the row R": `In`; `Has` goes. (3) One spelling of a row: `+:`;
      `+` goes (TestPStateAsync's `Async + Reader % Int`, README).
