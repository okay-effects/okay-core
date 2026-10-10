package okay.scala2

import cats.StackSafeMonad
import okay.core._

/** cats over `Program[R, *]` for Scala 2, against cats' 2.13 artifacts: `import okay.scala2.CatsProgram._` */
object CatsProgram {
  implicit def monad[R <: Row]: StackSafeMonad[Of[R]#L] = new StackSafeMonad[Of[R]#L] {
    def pure[A](a: A): Program[R, A] = Program.pure(a)
    def flatMap[A, B](fa: Program[R, A])(f: A => Program[R, B]): Program[R, B] = fa.flatMap(f)
  }
}
