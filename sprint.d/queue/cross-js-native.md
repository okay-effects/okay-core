# cross-js-native - the core compiled for the JVM, Scala.js and Scala Native

sbt-crossproject, Scala.js and Scala Native plugins at okay's versions. JVM only: JMH, `scala2/`, the JVM's options.
Tests split: shared (on all three) and JVM-only (threads, `Cps0` on a large stack, callbacks from other threads).
Sources moved to the cross layout — a large, mechanical diff.

Done when: `sbt test` runs the shared suites on JVM, JS and Native; benchmarks as before.
