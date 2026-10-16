# map-fused — a map of a map one map

`Cont.map` on a `Map` fuses: one map, its functions composed (`Cont.Fused`, `g` after `f`), up to `MaxFused` (32)
functions — the composition's calls nest, one host frame each, so it is bounded; past it a map of its own. At the
run a chain of maps is one frame and one value, not a frame, a node and a `Return` a map. (A map of an operation was
already its `MapK`; the machine's pairing of two map frames stays, for maps not made by `map`.)

Measured, JDK 26, `-wm BULK` (each fork warms every lane), µs/op, master → this: kernel capture 0.976 → 0.800,
captureDeep 11.68 → 7.41, facade capture 1.025 → 0.849; captureShallow, choose, queens, generate, orders within
their noise. MixedBenchmark makes no chain of maps: its allocation per run is the same to the byte, its single-lane
time the same (249.8 → 248.6 µs); under BULK its lanes scatter ±25% on either build. `TestFused`: 100 000 maps in
constant stack, the bound, a throw in a fused chain (the maps after it not run), a shift's `k` under 50 fused maps.
