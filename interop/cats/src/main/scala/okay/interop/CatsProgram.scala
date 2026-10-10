package okay.interop

import cats.StackSafeMonad
import okay.core.*

/** CATS OVER `Program[R, *]`: a program over a row is an `F[_]` — `Program.Of[R]` — and its monad cats', stack safe as
 * okay's monads are. `import okay.interop.CatsProgram.given` */
object CatsProgram:
  given monad[R <: Row]: StackSafeMonad[Program.Of[R]] = new StackSafeMonad[Program.Of[R]]:
    def pure[A](a: A): Program[R, A] = Program.pure(a)
    def flatMap[A, B](fa: Program[R, A])(f: A => Program[R, B]): Program[R, B] = fa.flatMap(f)
