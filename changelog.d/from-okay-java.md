## from-okay-java - what okay-java, the Java port on shift0, brought back

- `TestAgree`: 1 000 programs made at random, written once over the interface: writer, state, choose, throws and
  reader handlers nested in any order, resets and shifts, operations of the handlers in scope. They run on `Cont`,
  `Cont0`, `Ref` and `Cps0` and must agree, and they do. The paths to the effects are made as the handlers nest, at run
  time (the test's casts, not the library's). A reset adds no effect to the row, so a path counts the handlers
  between, not the resets. A shift is made only where its nearest delimiter is a reset, since up to a handler `Cont`
  and `Cont0` differ by design. In okay-java the same test caught each mutation planted in the machine to check it.
- `TestGuideDoc`: the guide's own text compiled. `project/GuideDoc.scala`, a test source generator, makes
  `docs/guide.md`'s `scala` blocks one object, `GuideDoc`: imports first, declarations as members. Each line
  `expr   // value` becomes a check (`the same` the value before, a trailing `, on Cont` not part of it). A guide
  line neither a declaration nor checked fails the build. Three blocks changed to be run as they read: the row a
  `type`, `twice` and `modified` run, `pstate` run. 12 checks; a value changed in the guide fails the test.
- The JVM a benchmark ran on: `bench` prints it, and `bench save` keeps it in the index's new `jvm` column. The
  README said JDK 25, but the runs in `bench.d` were on JDK 26, sbt's own (sdkman's current). okay-java's comparison
  of the two ran on different JDKs until it was found.
- Not brought: okay-java's capability that answers in place with no lookup (Scala's rows find a handler by path), its
  static ban on leaks (a path is not a value to leak), and its findings on `Integer.valueOf` against escape analysis
  (Scala's box elimination works in this machine's loop).
