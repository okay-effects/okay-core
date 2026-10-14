package okay.scala2

import cats.StackSafeMonad
import okay.one.Cont0
import okay.one.row.Row

/** CATS OVER `Of[R]#L` FROM SCALA 2: the same instance as `okay.interop.CatsProgram`'s, compiled by scalac 2.13 against
 * cats' 2.13 artifact. `import okay.scala2.CatsProgram._` */
object CatsProgram {
  implicit def monad[R <: Row]: StackSafeMonad[Of[R]#L] = new StackSafeMonad[Of[R]#L] {
    def pure[A](a: A): Cont0[R, Nothing, Nothing, A] = Cont0.pure(a)
    def flatMap[A, B](fa: Cont0[R, Nothing, Nothing, A])(f: A => Cont0[R, Nothing, Nothing, B]): Cont0[R, Nothing, Nothing, B] = okay.core.Ops.flatMap(fa)(f)
  }
}
