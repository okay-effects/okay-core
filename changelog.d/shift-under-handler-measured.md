## shift-under-handler-measured - a capture under a handler, tested and measured; Reset keeps its ret

- TestContext: a shift under `writer`, no reset between, resumed twice —
  `k` is the rest up to the handler with its `ret`.
- CaptureHandledBenchmark: CaptureBenchmark's capture through `depth`
  maps, `shots` resumptions, under an answering handler instead of a reset.
- It caught 95df988 ("Reset only a delimiter"), measured free only where no
  handler delimits: there the handler's `ret`, now a `Map` in the resumed
  body, lay under every resumed stack, so `Push` could not take `k` as the
  stack and resumed it frame by frame, a `Push` and a `Then` a frame a shot.
  captureHandled depth=256 shots=8: 5.98 µs / 46 984 B before, 13.63 µs /
  162 440 B after (×3.5 B); depth=1 shots=1: 456 B → 600 B. Reverted
  (49aa8d1): Reset(body, ret) again.
- Baseline kept, bench.d/2026-10-08T08-11-06_49aa8d1+.csv: allocation equal
  to 64c2929 in all 28 old benchmarks; captureHandled 30.7 ns / 456 B
  (1×1) … 5.81 µs / 46 984 B (256×8).
