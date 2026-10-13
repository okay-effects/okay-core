package okay.interop

import cats.StackSafeMonad
import okay.core.*

/** CATS OVER `Eff[R, *]`: a program over a row is an `F[_]` — `Eff[R, *]` — and its monad cats', stack safe as the
 * kernel's machine is. `import okay.interop.CatsProgram.given` */
object CatsProgram:
  type Of[R <: Row] = [A] =>> Eff[R, A]
  given monad[R <: Row]: StackSafeMonad[Of[R]] = new StackSafeMonad[Of[R]]:
    def pure[A](a: A): Eff[R, A] = okay.core.pure(a)
    def flatMap[A, B](fa: Eff[R, A])(f: A => Eff[R, B]): Eff[R, B] = fa.flatMap(f)
