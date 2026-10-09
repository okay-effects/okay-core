## benchmarks-one-suite - one suite on both monads; queens measuring the monad

- MonadsBenchmark: the same programs over the interface on Cont and Cont0 (`backend=cont|zero`), master's Cont
  benchmarks' programs among them (tail calls, binds, maps, Fibonacci, `A ! Es`, crossings, state, writer, throws,
  choose, collect, generate — the consumer's type each monad's own —, queens, captures under a reset and a handler).
  Bind, Capture, CaptureHandled, Cont, Depth, Effects, Fib, Queens benchmarks gone; DispatchBenchmark keeps Cont's
  own and the plain baseline; ZeroShift0Benchmark Cont0's `shift0`.
- queens: `safe` a loop, not `zipWithIndex` — its pairs, made or not as the JIT inlined it, were most of a run and
  its spread (540 to 1300 µs on both monads, fork to fork); now Cont 247–255, Cont0 188–200, 8 forks each.
- Measured, the full run kept in bench.d (2026-10-09T09-19-22_8d3b4f5+), ns an operation, Cont / Cont0: pure 3.27 /
  3.24, answered 6.50 / 5.43, general 8.99 / 7.72, across 1 / 4 / 16 handlers 11.7 / 22.8 / 90.4 against 5.7 /
  6.8 / 17.2, state 15.5 / 14.1, `!` 16.1 / 14.6, writer 20.0 / 17.3, throws 13.6 / 14.6, choose 29.1 / 33.9,
  collect 13.1 / 10.5, generate 14.5 / 14.1, fib 4.36 / 3.92; a run: queens 255 / 191 µs, a shift under a reset 16
  maps 8 shots 439 / 323 ns, under a handler 411 / 366, shallow under a handler 122 / 139. Cont0 behind: choose,
  throws, a shallow shift under a handler.
