# cont-loop-small — Cont's loop small enough for the JIT again

The full run after okay-interface showed `MixedBenchmark.orders` on `Cont` at 190 ns an order, 137 when the
benchmark came in. Bisected: `cont-faster` (74e895f) took it to 180 — its `run`, with `leave` inlined at every
delimiter case and the operation's dispatch written twice (`Inject`, `Suspend`), was 5215 bytes of bytecode, the old
one 3136. Not the nesting: with `Deep = 0` it was 182.

The dispatch out of the loop, two plain methods returning the program that leaves the delimiter — `returned` (a
value, its `ret`) and `performed` (an operation not answered in place) — `leave` called once each: `run` 2515 bytes.

Measured, JDK 26, `-f 2 -wi 5 -i 5`, cont: orders 190.0 → 142.8 µs, queens 178.6 → 159.8, choose 25.4 → 25.7,
forwarded16 17.35 → 17.36, general 10.3 → 9.9, answered 5.51 → 5.50.
