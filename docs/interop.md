# tofu and cats — the modules `interop/cats`, `interop/tofu`

A program over a row is an `F[_]`, `Program.Of[R]`. The modules give it the typeclasses — the core names no library:

- `okay-core-cats` (every platform): `okay.interop.CatsProgram`, cats' `StackSafeMonad`.
- `okay-core-tofu` (the JVM's: tofu has no JS or Native artifacts): `okay.interop.TofuProgram` — `WithLocal` from a
  `Reader` in the row (`local` its interceptor), `Errors` from a `Throws` (`handleWith` its interceptor), each `fa`
  built again under it, which a `Program` is for.

Every example here is compiled and run by `okay-core-tofu`'s `TestInteropDoc`.

```scala
import okay.core.*
import okay.std.*
import okay.interop.CatsProgram.given
import okay.interop.TofuProgram.given

def greeting[F[_]: cats.Monad](using ctx: tofu.WithLocal[F, String], err: tofu.Errors[F, String]): F[String] =
  cats.Monad[F].flatMap(ctx.local(ctx.context)(_.toUpperCase))(n => if n.isEmpty then err.raise("nobody") else cats.Monad[F].pure(s"hi $n"))

type Row2 = Reader % String +: Throws % String +: Pure

Program.run(okay.zero.top)(Programs.throws[String](Programs.reader("ann")(greeting[Program.Of[Row2]])))   // Right("hi ANN")
```

For Scala 2: the same, compiled by scalac 2.13 against cats' and tofu's 2.13 artifacts — `okay.scala2.CatsProgram`,
`okay.scala2.TofuProgram`, with the short names of `import okay.scala2._`.
