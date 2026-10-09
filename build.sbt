scalaVersion := "3.9.0"

lazy val unsafe = "--sun-misc-unsafe-memory-access=allow"
lazy val allowUnsafe = javaOptions ++= (if (fork.value) Seq(unsafe) else Nil)

lazy val root = (project in file("."))
  .enablePlugins(JmhPlugin)
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

// `sbt bench`, or `sbt "bench -f 1 -p n=100000 tailcall"`: JMH's options and a regexp go to `benchRun` (JMH,
// then a summary table: project/Bench.scala). `save` among them keeps the run in bench.d, `sbt "bench save"`;
// `sbt "benchHistory <regexp>"` reads it. `Jmh/compile` first, a command of its own — on a clean build the
// generator runs before the benchmarks are compiled and finds none
addCommandAlias("bench", "Jmh/compile; benchRun")
