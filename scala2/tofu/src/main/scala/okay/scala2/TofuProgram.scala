package okay.scala2

import cats.Functor
import okay.core.Intercept
import okay.one.Cont0
import okay.core.Ops._
import okay.one.row.{In, Row}
import okay.one.std.{Reader, Throws}
import tofu.{Errors, WithLocal}

/** tofu's typeclasses over `Of[R]#L` for Scala 2, against tofu's 2.13 artifacts — as `okay.interop.TofuProgram`'s,
 * by `Intercept.at`: `import okay.scala2.CatsProgram._, okay.scala2.TofuProgram._` */
object TofuProgram {
  type P[R <: Row, A] = Cont0[R, Nothing, Nothing, A]

  implicit def withLocal[R <: Row, C](implicit in: In[ReaderOf[C]#L, R]): WithLocal[Of[R]#L, C] = new WithLocal[Of[R]#L, C] {
    def functor: Functor[Of[R]#L] = CatsProgram.monad[R]
    def context: P[R, C] = okay.core.Ops.perform[ReaderOf[C]#L, R, C](Reader.Ask[C]())(in)
    def local[A](fa: P[R, A])(project: C => C): P[R, A] =
      okay.core.Ops.flatMap(context)((c: C) => Intercept.by[ReaderOf[C]#L, R, A](in)(fa)(new Intercept.Clause[ReaderOf[C]#L, R, A] {
        def apply[X](op: Reader[C, X], k: X => P[R, A]): P[R, A] = op match { case _: Reader.Ask[c] => k(project(c)) }
      }))
  }

  implicit def errors[R <: Row, E](implicit in: In[ThrowsOf[E]#L, R]): Errors[Of[R]#L, E] = new Errors[Of[R]#L, E] {
    def raise[A](err: E): P[R, A] = okay.core.Ops.perform[ThrowsOf[E]#L, R, Nothing](Throws.Raise[E](err))(in)
    def tryHandleWith[A](fa: P[R, A])(f: E => Option[P[R, A]]): P[R, A] =
      Intercept.by[ThrowsOf[E]#L, R, A](in)(fa)(new Intercept.Clause[ThrowsOf[E]#L, R, A] {
        def apply[X](op: Throws[E, X], k: X => P[R, A]): P[R, A] = op match { case r: Throws.Raise[e] => f(r.e).getOrElse(raise[A](r.e)) }
      })
    def restore[A](fa: P[R, A]): P[R, Option[A]] =
      Intercept.by[ThrowsOf[E]#L, R, Option[A]](in)(okay.core.Ops.map(fa)(a => Option(a)))(new Intercept.Clause[ThrowsOf[E]#L, R, Option[A]] {
        def apply[X](op: Throws[E, X], k: X => P[R, Option[A]]): P[R, Option[A]] = Cont0.pure[R, Option[A]](None)
      })
    def lift[A](fa: P[R, A]): P[R, A] = fa
  }
}
