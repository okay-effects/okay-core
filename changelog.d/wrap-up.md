# wrap-up — the sprint's full run, README

The sprint (sprint.d): std without java.util.concurrent, async a module of its own, the core on the JVM, Scala.js
and Scala Native, effects written once, Scala 2 handlers, optional modules (cats, tofu, Scala 2), Cont's loop in
Cont0's shape, then `A ! E1 + E2` and the `okay` package, okay's words and okay's interface (`RowEffects`).

The full run, `bench save -prof gc`, JDK 26 (bench.d/2026-10-10T14-20-56_6511bb7+.csv), against the one before it
(2026-10-10T13-40-47, after okay-interface): MixedBenchmark cont −20% (cont-loop-small), queens cont −11%, general
zero −17%; the rest within noise. README's table from it; tests on the three platforms and Scala 2 green.
