package okay.core.std

import okay.core.*
import okay.one.handler.Deep

/** READER: the environment, answered in place — the kernel's std's effect */
export okay.one.std.Reader

def ask[E]: Op[Reader % E, E] = perform(Reader.Ask[E]())
/** `reader(e)(body)`: every `ask` answered `e` */
def reader[E, A, T <: Row](e: E)(body: Under[Reader % E, T, A]): Eff[T, A] = handleIn(Reader[E, T, A](e))(body)
/** `local(f)(body)`: every `ask` of the body answered `f` of the value outside — the reader intercepted */
def local[E, A, T <: Row](f: E => E)(body: Under[Reader % E, T, A])(using Path[Reader % E, T]): Eff[T, A] =
  intercept(new Deep[Reader % E, T, A, A]:
    def ret(a: A): Eff[T, A] = pure(a)
    def apply[X](op: Reader[E, X], k: X => Eff[T, A]): Eff[T, A] = op match
      case Reader.Ask() => ask[E].at[T].flatMap(v => k(f(v))))(body)
