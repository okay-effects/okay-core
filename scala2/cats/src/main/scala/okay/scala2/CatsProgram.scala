package okay.scala2

import cats.StackSafeMonad
import okay.one.Cont
import okay.one.row.Row

/** CATS OVER `Of[R]#L` FROM SCALA 2: the same instance as `okay.interop.CatsProgram`'s, compiled by scalac 2.13 against
 * cats' 2.13 artifact. `import okay.scala2.CatsProgram._` */
object CatsProgram {
  implicit def monad[R <: Row]: StackSafeMonad[Of[R]#L] = new StackSafeMonad[Of[R]#L] {
    def pure[A](a: A): Cont[R, Nothing, Nothing, A] = Cont.pure(a)
    def flatMap[A, B](fa: Cont[R, Nothing, Nothing, A])(f: A => Cont[R, Nothing, Nothing, B]): Cont[R, Nothing, Nothing, B] = okay.core.Ops.flatMap(fa)(f)
  }
}
