## zero-one-cast - Cont0's empty cases one object each, by the one cast — throws 16.4 → 13.8 µs, generate −13%, captureOnce −28%

- `Frames.End`, `Meta.Top`, `KMeta.Top` and the machine's `Rst` hold nothing: their type parameters only tie the
  types of what they are in. Made each time before — an `End` each node entered and each shift, a `Meta.Top` a field
  of every node, a `KMeta.Top` each kernel run, an `Rst` each shift at a reset. Now one object each, at any types, by
  one documented cast, `empty` (`Cont0.scala`), its type always written: inferred, it is `Nothing`, and a cast to
  `Nothing` fails. `Delim.top` a method, a node one field less.
- `In.Here` left as it is: a path carries the lookup cache (`found`); one `Here` for all would be one cache for every
  effect at the head, overwritten in turn.
- Measured, Cont0, all of MonadsBenchmark, before / after: throwing 16.40 / 13.83 µs (232 → 200 B), generate
  15.28 / 13.31 (224 → 192 B), generateMapped 16.57 / 14.75, captureOnce 42.8 / 31.0 ns, captureShallow
  136.5 / 125.0, captureHandledGeneral 357 / 341; the rest within noise.
