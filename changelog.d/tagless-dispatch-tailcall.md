## tagless-dispatch-tailcall - the type classes' cost, split: the core is as fast, the path through the instance ~1.2 ns

- ContBenchmark.tailcallHandledCont: tailcallHandled on the concrete `Cont`,
  no context — `inject` and `Cont.flatMap` straight, the same answering
  handler. Back to back, two rounds (12:28–12:30, a loaded machine, load ~11):
  tailcallHandled 54.6 / 55.6 µs at 75a3ddf; at HEAD 68.0 / 66.7 through the
  classes, 58.7 / 56.7 on the concrete `Cont`. So the core (fold, loop) did
  not change — its code is the same but for type parameters erased — and the
  ~20% is the way through the instance in the program: `perform` →
  `c.perform` → `At.perform`, `flatMap` → `c.flatMap` → `At.flatMap`, with
  the bridges erasure makes (`At` returns `Cont` where the class returns the
  abstract `M`).
- Tried, no effect, reverted: Cont.flatMap smaller than 35 bytes (61 → 32);
  `At.flatMap` building the nodes itself, one call less.
- Where it stands: ~1.2 ns an operation, seen only where an operation costs
  ~5.5 ns (answered in place, nothing else per step); stateGetPut, writer,
  choose, forwarding, captures equal within noise. A hot loop can be written
  on `Cont` itself, as tailcallHandledCont is, and run at the core's speed.
- Noted for any later measuring: a second session benchmarked on this machine
  at the same time (one JMH lock, one CPU), so numbers from 12:00 on were
  taken back to back only, never against an older run.
