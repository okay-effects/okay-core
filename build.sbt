ThisBuild / scalaVersion := "3.9.0"
// okay-kernel's API, the one dependency of the core: published locally (okay-kernel: `sbt publishM2`)
ThisBuild / resolvers += Resolver.mavenLocal
lazy val kernelVersion = "0.2.0-SNAPSHOT"

/** a platform's own sources beside the shared ones: `src/<config>/scala-<name>`, as okay's modules have them */
def platformDirs(config: String, names: String*) = Def.setting(
  names.map(n => baseDirectory.value.getParentFile / "src" / config / s"scala-$n"))

/**
 * THE CORE, CROSS-BUILT: the JVM, Scala.js, Scala Native — one source tree, `src/`, shared; a platform's own beside
 * it (`src/test/scala-jvm`, `-js`, `-jvm-native`). The JVM's alone: the benchmarks (JMH), docs/guide.md's checked
 * blocks, the JVM's options. Its one dependency, okay-kernel's API
 * (docs/kernel.md): its Java jar on the JVM, its Scala mirror on JS and Native — the machines run the kernel's
 * programs natively
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
    libraryDependencies ++= Seq("dev.okay" % "okay-kernel-api" % kernelVersion,
      // okay-kernel's reference machine: KernelMixedBenchmark's yardstick, the same program on it
      "dev.okay" %% "okay-kernel-steps" % kernelVersion % Test),
    Test / unmanagedSourceDirectories ++= platformDirs("test", "jvm", "jvm-native").value,
    // sun.misc.Unsafe's memory access allowed in every forked JVM — run and test when forked, the JMH host
    // (always forked) and its benchmark forks, which inherit the host's options: JDK 24+ warns on the first call
    // (JEP 498), JMH 1.37 makes one. Only where forked: sbt warns of javaOptions it would ignore. sbt's own JVM,
    // where the unforked tasks run, gets it from .jvmopts
    inConfig(Compile)(allowUnsafe),
    inConfig(Test)(allowUnsafe),
    Jmh / sourceDirectory := baseDirectory.value.getParentFile / "src" / "jmh",
    Jmh / run / javaOptions ~= (o => (o :+ unsafe).distinct),
    // the suites forked on a small stack: the kernel's constant-stack guarantee (100 000 handlers, resets) fails loudly
    Test / fork := true,
    Test / javaOptions += "-Xss512k",
    Bench.settings
  )
  .jsSettings(
    libraryDependencies += "dev.okay" %%% "okay-kernel-scala" % kernelVersion,
    Test / unmanagedSourceDirectories ++= platformDirs("test", "js").value
  )
  .nativeSettings(
    libraryDependencies += "dev.okay" %%% "okay-kernel-scala" % kernelVersion,
    Test / unmanagedSourceDirectories ++= platformDirs("test", "jvm-native").value
  )

/**
 * THE WORDS, `import okay.*`: `A ! E1 + E2`, the standard effects by their names, `perform`, `run` — one package
 * object over `okay.core` and `okay.core.std`, an artifact of its own (`okay-core-words`), so that the core puts nothing
 * at the top of the package `okay`: the okay platform, whose own `okay.*` are its words over `Effects[M]`, depends on
 * the core without a clash on one classpath (docs/okay.md). A user of okay-core alone adds it for the taste. Its
 * tests: what is written on `import okay.*` alone, and docs/guide.md's blocks compiled as they are written
 */
lazy val words = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("words"))
  .dependsOn(core)
  .settings(
    name := "okay-core-words",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.2.3" % Test
  )
  .jvmSettings(
    Test / unmanagedSourceDirectories ++= platformDirs("test", "jvm").value,
    inConfig(Test)(allowUnsafe),
    Test / fork := true,
    // docs/guide.md's scala blocks, compiled as they are written and their `expr   // value` lines checked
    Test / sourceGenerators += Def.task {
      GuideDoc.generate(baseDirectory.value.getParentFile.getParentFile / "docs" / "guide.md", (Test / sourceManaged).value / "okay" / "docs" / "GuideDoc.scala")
    }.taskValue
  )

lazy val unsafe = "--sun-misc-unsafe-memory-access=allow"
lazy val allowUnsafe = javaOptions ++= (if (fork.value) Seq(unsafe) else Nil)

/** the build's root: every platform's core and async, and the Scala 2.13 suites — `sbt test` runs them all */
lazy val root = (project in file("."))
  .aggregate(core.jvm, core.js, core.native, words.jvm, words.js, words.native, async.jvm, async.js, async.native, tx.jvm, tx.js, tx.native,
    interopCats.jvm, interopCats.js, interopCats.native, interopTofu, scala2Prelude, scala2Cats, scala2Tofu, scala2, kernel)
  .settings(
    name := "okay-core-root",
    publish / skip := true,
    Compile / sources := Seq(),
    Test / sources := Seq()
  )

/**
 * TRANSACTIONS, A MODULE OF ITS OWN (okay.tx), cross-built as the core: `Tx`, a transaction as a subtree — `Atomic`
 * a bracket holding its body, `Rollback` a leaf — its handler the resource's owner (`Transactor`), walking the tree
 * with one bit; the words `atomic`, `rollback`, `transactions`. No database in it: an owner for JDBC and the like
 * is the platform's
 */
lazy val tx = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("tx"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(
    name := "okay-core-tx",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.2.3" % Test
  )
  .jvmSettings(
    // the JVM's own: a transactor over JDBC, its test on H2
    Compile / unmanagedSourceDirectories ++= platformDirs("main", "jvm").value,
    Test / unmanagedSourceDirectories ++= platformDirs("test", "jvm").value,
    libraryDependencies += "com.h2database" % "h2" % "2.3.232" % Test
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
 * SCALA 2.13 READING THE CORE: a project compiled by scalac 2.13.18 against the core's TASTy (`-Ytasty-reader`).
 * Two standard libraries, in this ORDER (okay's finding, okay/specs/scala2-facade.md): 2.13's first to compile —
 * Scala 3.9's carries TASTy, not Scala 2 pickles — and 3.9's behind it, needed to compile and to run
 * (`scala.reflect.Enum`). The core's own dependency on 3.9's stdlib is excluded and appended at the end
 */
def scala2Settings: Seq[Def.Setting[?]] = Seq(
  scalaVersion := "2.13.18",
  projectDependencies ~= (_.map(_.exclude("org.scala-lang", "scala-library").exclude("org.scala-lang", "scala3-library_3"))),
  scalacOptions := Seq("-Ytasty-reader", "-deprecation", "-feature", "-Xlint"),
  ivyConfigurations += Scala3Stdlib,
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-library" % "3.9.0" % Scala3Stdlib,
    "org.scala-lang" % "scala3-library_3" % "3.9.0" % Scala3Stdlib)
) ++ Seq(Compile, Runtime, Test).flatMap(c => Seq(
  c / dependencyClasspath ++= Classpaths.managedJars(Scala3Stdlib, Set("jar"), update.value),
  c / dependencyClasspathAsJars ++= Classpaths.managedJars(Scala3Stdlib, Set("jar"), update.value)))

/** okay-core's short names for Scala 2 (`ReaderOf[E]#L`, `Of[R]#L`): a package object compiled by scalac 2.13 */
lazy val scala2Prelude = (project in file("scala2/prelude"))
  .dependsOn(core.jvm)
  .settings(scala2Settings, name := "okay-core-scala2-prelude")

/** cats over `Program` for Scala 2, against cats' 2.13 artifacts */
lazy val scala2Cats = (project in file("scala2/cats"))
  .dependsOn(scala2Prelude)
  .settings(scala2Settings, name := "okay-core-scala2-cats", libraryDependencies += "org.typelevel" %% "cats-core" % "2.13.0")

/** tofu over `Program` for Scala 2, against tofu's 2.13 artifacts */
lazy val scala2Tofu = (project in file("scala2/tofu"))
  .dependsOn(scala2Cats)
  .settings(scala2Settings, name := "okay-core-scala2-tofu", libraryDependencies += "tf.tofu" %% "tofu-kernel" % "0.14.3")

/** THE PROOF that the core and its modules stay readable from Scala 2: suites in Scala 2.13 — one's own effects and
 * handlers, tofu over `Program` — on the one monad */
lazy val scala2 = (project in file("scala2/tests"))
  .dependsOn(scala2Tofu)
  .settings(scala2Settings, name := "okay-core-scala2-tests", publish / skip := true,
    libraryDependencies += "org.scalameta" %% "munit" % "1.2.3" % Test)

/** cats over `Program` (okay.interop.CatsProgram), a module of its own: the core names no library. Cross-built, as
 * cats is */
lazy val interopCats = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("interop/cats"))
  .dependsOn(core)
  .settings(name := "okay-core-cats", libraryDependencies ++= Seq(
    "org.typelevel" %%% "cats-core" % "2.13.0", "org.scalameta" %%% "munit" % "1.2.3" % Test))

/** tofu over `Program` (okay.interop.TofuProgram): the JVM's — tofu has no JS or Native artifacts. Its suites on the
 * four monads (the core's test helpers), docs/interop.md's blocks compiled and checked */
lazy val interopTofu = (project in file("interop/tofu"))
  .dependsOn(interopCats.jvm, core.jvm % "test->test")
  .settings(
    name := "okay-core-tofu",
    libraryDependencies ++= Seq("tf.tofu" %% "tofu-kernel" % "0.14.3", "org.scalameta" %% "munit" % "1.2.3" % Test),
    inConfig(Test)(allowUnsafe),
    Test / sourceGenerators += Def.task {
      GuideDoc.generate(baseDirectory.value.getParentFile.getParentFile / "docs" / "interop.md", (Test / sourceManaged).value / "okay" / "interop" / "InteropDoc.scala", "okay.interop", "InteropDoc")
    }.taskValue
  )

/**
 * OKAY-KERNEL'S CHECKS ON OKAY-CORE (docs/kernel.md): the kernel's `Effects`, native in the core
 * (`okay.core.kernel.OnCore`), checked by okay-kernel's TCK and against its reference machine — the
 * JVM's, as they are. Tests only: nothing of its own to publish
 */
lazy val kernel = (project in file("kernel"))
  .dependsOn(core.jvm)
  .settings(
    name := "okay-core-kernel",
    publish / skip := true,
    libraryDependencies ++= Seq(
      "dev.okay" %% "okay-kernel-steps" % kernelVersion % Test,
      "dev.okay" %% "okay-kernel-tck" % kernelVersion % Test,
      "org.scalameta" %% "munit" % "1.2.3" % Test)
  )

// `sbt bench`, or `sbt "bench -f 1 -p n=100000 tailcall"`: JMH's options and a regexp go to `benchRun` (JMH,
// then a summary table: project/Bench.scala). `save` among them keeps the run in bench.d, `sbt "bench save"`;
// `sbt "benchHistory <regexp>"` reads it. `Jmh/compile` first, a command of its own — on a clean build the
// generator runs before the benchmarks are compiled and finds none
addCommandAlias("bench", "Jmh/compile; benchRun")
