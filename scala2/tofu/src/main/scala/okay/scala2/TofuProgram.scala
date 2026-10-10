package okay.scala2

import cats.Functor
import okay.core._
import okay.std._
import tofu.{Errors, WithLocal}

/** tofu's typeclasses over `Program[R, *]` for Scala 2, against tofu's 2.13 artifacts:
 * `import okay.scala2.CatsProgram._, okay.scala2.TofuProgram._` */
object TofuProgram {
  implicit def withLocal[R <: Row, C](implicit in: In[ReaderOf[C]#L, R]): WithLocal[Of[R]#L, C] = new WithLocal[Of[R]#L, C] {
    def functor: Functor[Of[R]#L] = CatsProgram.monad[R]
    def context: Program[R, C] = Programs.ask[R, C]
    def local[A](fa: Program[R, A])(project: C => C): Program[R, A] = Programs.local[R, C, A](project)(fa)
  }

  implicit def errors[R <: Row, E](implicit in: In[ThrowsOf[E]#L, R]): Errors[Of[R]#L, E] = new Errors[Of[R]#L, E] {
    def raise[A](err: E): Program[R, A] = Programs.raise[R, E, A](err)
    def tryHandleWith[A](fa: Program[R, A])(f: E => Option[Program[R, A]]): Program[R, A] =
      Programs.handleWith[R, E, A](fa)(e => f(e).getOrElse(Programs.raise[R, E, A](e)))
    def restore[A](fa: Program[R, A]): Program[R, Option[A]] =
      Programs.handleWith[R, E, Option[A]](fa.map(a => Option(a)))(_ => Program.pure[R, Option[A]](None))
    def lift[A](fa: Program[R, A]): Program[R, A] = fa
  }
}
