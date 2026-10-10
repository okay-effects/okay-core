scalaVersion := "3.9.0"

lazy val unsafe = "--sun-misc-unsafe-memory-access=allow"
lazy val allowUnsafe = javaOptions ++= (if (fork.value) Seq(unsafe) else Nil)

lazy val root = (project in file("."))
  .enablePlugins(JmhPlugin)
  // `sbt test` runs the Scala 2.13 suites too: the core read from Scala 2 is gated with the rest
  .aggregate(LocalProject("scala2"), LocalProject("async"))
  .settings(
    name := "okay-core",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.2.3" % Test,
      // tofu's typeclasses over okay's `Program` (src/test/scala/okay/tofu): test scope — the core names no library.
      // tofu 0.14.3's Scala 3 artifacts (built on 3.3) read by 3.9; cats-core the one tofu-kernel is built against
      "tf.tofu" %% "tofu-kernel" % "0.14.3" % Test,
      "org.typelevel" %% "cats-core" % "2.13.0" % Test
    ),
    // sun.misc.Unsafe's memory access allowed in every forked JVM — run and test when forked, the JMH host
    // (always forked) and its benchmark forks, which inherit the host's options: JDK 24+ warns on the first call
    // (JEP 498), JMH 1.37 makes one. Only where forked: sbt warns of javaOptions it would ignore. sbt's own JVM,
    // where the unforked tasks run, gets it from .jvmopts
    inConfig(Compile)(allowUnsafe),
    inConfig(Test)(allowUnsafe),
    Jmh / run / javaOptions ~= (o => (o :+ unsafe).distinct),
    Bench.settings,
    // docs/guide.md's scala blocks, compiled as they are written and their `expr   // value` lines checked
    Test / sourceGenerators += Def.task {
      GuideDoc.generate(baseDirectory.value / "docs" / "guide.md", (Test / sourceManaged).value / "okay" / "docs" / "GuideDoc.scala")
    }.taskValue
  )

/**
 * ASYNC, A MODULE OF ITS OWN (okay.async): `async`, `await`, `Async.run`, asynchronous streams, and what they wait
 * and race with (`Swap`, `Mailbox`, `Platform`) — so the core has no thread, no wait, no callback in it. Its tests
 * use the core's test helpers (the backends, the references); docs/async.md's blocks compiled and checked as the
 * guide's are; its benchmark its own (`async/Jmh/run`)
 */
lazy val async = (project in file("async"))
  .enablePlugins(JmhPlugin)
  .dependsOn(root % "compile->compile;test->test")
  .settings(
    name := "okay-core-async",
    scalaVersion := "3.9.0",
    libraryDependencies += "org.scalameta" %% "munit" % "1.2.3" % Test,
    inConfig(Test)(allowUnsafe),
    Jmh / run / javaOptions ~= (o => (o :+ unsafe).distinct),
    Test / sourceGenerators += Def.task {
      GuideDoc.generate((root / baseDirectory).value / "docs" / "async.md", (Test / sourceManaged).value / "okay" / "async" / "AsyncDoc.scala", "okay.async", "AsyncDoc")
    }.taskValue
  )

/** Scala 3's stdlib for the Scala 2.13 project, resolved here and placed BEHIND 2.13's by hand */
lazy val Scala3Stdlib = config("scala3Stdlib").hide

/**
 * OKAY-CORE FROM SCALA 2.13: suites written in Scala 2 and compiled by scalac 2.13.18, reading the core's TASTy
 * (`-Ytasty-reader`) — the proof that the core stays readable from Scala 2, and tofu's Scala 2 artifacts over it.
 * Two standard libraries, in this ORDER (okay's finding, okay/specs/scala2-facade.md): 2.13's first to compile —
 * Scala 3.9's carries TASTy, not Scala 2 pickles — and 3.9's behind it, needed to compile and to run
 * (`scala.reflect.Enum`). The core's own dependency on 3.9's stdlib is excluded and appended at the end.
 */
lazy val scala2 = (project in file("scala2"))
  .dependsOn(root)
  .settings(
    name := "okay-core-scala2",
    publish / skip := true,
    scalaVersion := "2.13.18",
    projectDependencies ~= (_.map(_.exclude("org.scala-lang", "scala-library").exclude("org.scala-lang", "scala3-library_3"))),
    scalacOptions := Seq("-Ytasty-reader", "-deprecation", "-feature", "-Xlint"),
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.2.3" % Test,
      "tf.tofu" %% "tofu-kernel" % "0.14.3" % Test,
      "org.typelevel" %% "cats-core" % "2.13.0" % Test
    ),
    ivyConfigurations += Scala3Stdlib,
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-library" % "3.9.0" % Scala3Stdlib,
      "org.scala-lang" % "scala3-library_3" % "3.9.0" % Scala3Stdlib),
    Seq(Compile, Runtime, Test).flatMap(c => Seq(
      c / dependencyClasspath ++= Classpaths.managedJars(Scala3Stdlib, Set("jar"), update.value),
      c / dependencyClasspathAsJars ++= Classpaths.managedJars(Scala3Stdlib, Set("jar"), update.value)))
  )

// `sbt bench`, or `sbt "bench -f 1 -p n=100000 tailcall"`: JMH's options and a regexp go to `benchRun` (JMH,
// then a summary table: project/Bench.scala). `save` among them keeps the run in bench.d, `sbt "bench save"`;
// `sbt "benchHistory <regexp>"` reads it. `Jmh/compile` first, a command of its own — on a clean build the
// generator runs before the benchmarks are compiled and finds none
addCommandAlias("bench", "Jmh/compile; benchRun")
