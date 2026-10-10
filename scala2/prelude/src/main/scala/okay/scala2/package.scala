package okay

import okay.core.Program

/** OKAY-CORE'S SHORT NAMES FOR SCALA 2, compiled by scalac 2.13 — Scala 2 sees no Scala 3 top-level definition and
 * reads a type lambda alias (`%`) as a proper type, so an effect of two parameters is a Scala 2 type lambda:
 * `+:[ReaderOf[Int]#L, Pure]`, a program over a row as an `F[_]`, `Of[R]#L`. `import okay.scala2._` */
package object scala2 {
  type Of[R <: okay.core.Row] = { type L[A] = Program[R, A] }
  type ReaderOf[E] = { type L[X] = okay.std.Reader[E, X] }
  type StateOf[S] = { type L[X] = okay.std.State[S, X] }
  type WriterOf[W] = { type L[X] = okay.std.Writer[W, X] }
  type ThrowsOf[E] = { type L[X] = okay.std.Throws[E, X] }
  type EmitOf[W] = { type L[X] = okay.std.Emit[W, X] }
}
