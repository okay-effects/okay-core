# tofu and cats — the modules `interop/cats`, `interop/tofu`

A program over a row is an `F[_]`, `Eff[R, *]` (`CatsProgram.Of[R]`). The modules give it the typeclasses — the core names no library of them:

- `okay-core-cats` (every platform): `okay.interop.CatsProgram`, cats' `StackSafeMonad`.
- `okay-core-tofu` (the JVM's: tofu has no JS or Native artifacts): `okay.interop.TofuProgram` — `WithLocal` from a
  `Reader` anywhere in the row (`local` its interceptor), `Errors` from a `Throws` (`handleWith` its interceptor),
  each by `okay.core.Intercept.at`: the effect handled again where it is in the row, the row kept — the program
  stepped, an operation of the effect to the clause, any other performed again outside (the slow road, a run of the
  machine an operation; the kernel's own relocation for it is to come).

Every example here is compiled and run by `okay-core-tofu`'s `TestInteropDoc`.

```scala
import okay.core.*
import okay.std.*
import okay.interop.CatsProgram.given
import okay.interop.TofuProgram.given

def greeting[F[_]: cats.Monad](using ctx: tofu.WithLocal[F, String], err: tofu.Errors[F, String]): F[String] =
  cats.Monad[F].flatMap(ctx.local(ctx.context)(_.toUpperCase))(n => if n.isEmpty then err.raise("nobody") else cats.Monad[F].pure(s"hi $n"))

type Row2 = Reader % String +: Throws % String +: Pure

run(throws[String, String, Pure](reader("ann")(greeting[CatsProgram.Of[Row2]])))   // Right("hi ANN")
```

For Scala 2: the same, compiled by scalac 2.13 against cats' and tofu's 2.13 artifacts — `okay.scala2.CatsProgram`,
`okay.scala2.TofuProgram`, with the short names of `import okay.scala2._`.
