package okay.std

import okay.core.*

/** READER: the environment, answered in place */
enum Reader[E, +A]:
  case Ask[E]() extends Reader[E, E]

def ask[E](using c: Effects, has: Reader % E :? c.Effs): c.Prog[E] = perform[Reader % E, E](Reader.Ask[E]())
/** `reader[A](e)(body)`: every `ask` answered `e` */
def reader[A](using c: Effects)[E](e: E)(body: c.Body[Reader % E, A, A]): c.Prog[A] =
  Effects.handle(new Answering[c.Prog, Reader % E, A, A]:
    def ret(a: A): A = a
    def value[X](op: Reader[E, X]): X = op match
      case Reader.Ask() => e)(body)
/** `local[E, A](f)(body)`: every `ask` of the body answered `f` of the value outside — the reader intercepted */
def local[E, A](f: E => E)(using c: Effects, in: Reader % E :? c.Effs)(body: c.Intercepted[A, A]): c.Prog[A] =
  Effects.intercept(new Handler[c.Prog, Reader % E, A, A]:
    def ret(a: A): A = a
    def apply[X](op: Reader[E, X], k: X => c.Prog[A]): c.Prog[A] = op match
      case Reader.Ask() => ask[E].flatMap(v => k(f(v))))(body)
