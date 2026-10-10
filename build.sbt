ThisBuild / scalaVersion := "3.9.0"

/** a platform's own sources beside the shared ones: `src/<config>/scala-<name>`, as okay's modules have them */
def platformDirs(config: String, names: String*) = Def.setting(
  names.map(n => baseDirectory.value.getParentFile / "src" / config / s"scala-$n"))

/**
 * THE CORE, CROSS-BUILT: the JVM, Scala.js, Scala Native — one source tree, `src/`, shared; a platform's own beside
 * it (`src/test/scala-jvm`, `-js`, `-jvm-native`). The JVM's alone: the benchmarks (JMH), tofu's and cats' tests,
 * docs/guide.md's checked blocks (its tofu example), the JVM's options. On JS no suite of the textbook CPS (`Cps0`):
 * not stack safe, it needs a thread of a large stack, which JS has not
 */
lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("."))
  .settings(
    name := "okay-core",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.2.3" % Test
  )
  .jvmConfigure(_.enablePlugins(JmhPlugin))
  .jvmSettings(
    libraryDependencies ++= Seq(
      // tofu's typeclasses over okay's `Program` (src/test/scala-jvm/okay/tofu): test scope — the core names no
      // library. tofu 0.14.3's Scala 3 artifacts (built on 3.3) read by 3.9, the JVM's only; cats-core the one
      // tofu-kernel is built against
      "tf.tofu" %% "tofu-kernel" % "0.14.3" % Test,
      "org.typelevel" %% "cats-core" % "2.13.0" % Test
    ),
    Test / unmanagedSourceDirectories ++= platformDirs("test", "jvm", "jvm-native").value,
    // sun.misc.Unsafe's memory access allowed in every forked JVM — run and test when forked, the JMH host
    // (always forked) and its benchmark forks, which inherit the host's options: JDK 24+ warns on the first call
    // (JEP 498), JMH 1.37 makes one. Only where forked: sbt warns of javaOptions it would ignore. sbt's own JVM,
    // where the unforked tasks run, gets it from .jvmopts
    inConfig(Compile)(allowUnsafe),
    inConfig(Test)(allowUnsafe),
    Jmh / sourceDirectory := baseDirectory.value.getParentFile / "src" / "jmh",
    Jmh / run / javaOptions ~= (o => (o :+ unsafe).distinct),
    Bench.settings,
    // docs/guide.md's scala blocks, compiled as they are written and their `expr   // value` lines checked
    Test / sourceGenerators += Def.task {
      GuideDoc.generate(baseDirectory.value.getParentFile / "docs" / "guide.md", (Test / sourceManaged).value / "okay" / "docs" / "GuideDoc.scala")
    }.taskValue
  )
  .jsSettings(
    Test / unmanagedSourceDirectories ++= platformDirs("test", "js").value,
    Test / testOptions += Tests.Filter(!_.endsWith("Cps0"))
  )
  .nativeSettings(
    Test / unmanagedSourceDirectories ++= platformDirs("test", "jvm-native").value
  )

lazy val unsafe = "--sun-misc-unsafe-memory-access=allow"
lazy val allowUnsafe = javaOptions ++= (if (fork.value) Seq(unsafe) else Nil)

/** the build's root: every platform's core and async, and the Scala 2.13 suites — `sbt test` runs them all */
lazy val root = (project in file("."))
  .aggregate(core.jvm, core.js, core.native, async.jvm, async.js, async.native, LocalProject("scala2"))
  .settings(
    name := "okay-core-root",
    publish / skip := true,
    Compile / sources := Seq(),
    Test / sources := Seq()
  )

/**
 * ASYNC, A MODULE OF ITS OWN (okay.async), cross-built as the core: `async`, `await`, `Async.run`, asynchronous
 * streams, and what they wait and race with (`Swap`, `Mailbox`, `Platform` — one a platform: JS cannot wait) — so the
 * core has no thread, no wait, no callback in it. Its tests use the core's test helpers; those with threads the
 * JVM's; docs/async.md's blocks compiled and checked as the guide's are; its benchmark its own (`asyncJVM/Jmh/run`)
 */
lazy val async = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("async"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(
    name := "okay-core-async",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.2.3" % Test
  )
  .jvmConfigure(_.enablePlugins(JmhPlugin))
  .jvmSettings(
    Compile / unmanagedSourceDirectories ++= platformDirs("main", "jvm-native").value,
    Test / unmanagedSourceDirectories ++= platformDirs("test", "jvm").value,
    inConfig(Test)(allowUnsafe),
    Jmh / sourceDirectory := baseDirectory.value.getParentFile / "src" / "jmh",
    Jmh / run / javaOptions ~= (o => (o :+ unsafe).distinct),
    Test / sourceGenerators += Def.task {
      GuideDoc.generate(baseDirectory.value.getParentFile.getParentFile / "docs" / "async.md", (Test / sourceManaged).value / "okay" / "async" / "AsyncDoc.scala", "okay.async", "AsyncDoc")
    }.taskValue
  )
  .jsSettings(
    Compile / unmanagedSourceDirectories ++= platformDirs("main", "js").value
  )
  .nativeSettings(
    Compile / unmanagedSourceDirectories ++= platformDirs("main", "jvm-native").value
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
  .dependsOn(core.jvm)
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
