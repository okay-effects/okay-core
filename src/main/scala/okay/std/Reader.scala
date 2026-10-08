package okay.std

import okay.core.*

/** READER: the environment, answered in place */
enum Reader[E, +A]:
  case Ask[E]() extends Reader[E, E]

def ask[E](using c: Effects, has: Reader % E :? c.R): c.M[c.R, c.S, c.S, E] = perform[Reader % E, E](Reader.Ask[E]())
/** `reader[A](e)(body)`: every `ask` answered `e` */
def reader[A](using c: Effects)[E](e: E)(body: c.Under[Reader % E +: c.R, A] ?=> c.M[Reader % E +: c.R, A, A, A]): c.M[c.R, c.S, c.S, A] =
  Effects.handle(new Answering[c.M, Reader % E, c.R, c.S, A, A]:
    def ret(a: A): A = a
    def value[X](op: Reader[E, X]): X = op match
      case Reader.Ask() => e)(body)
