package okay.zero.std

import okay.zero.*

/** READER: the environment, answered in place */
enum Reader[E, +A]:
  case Ask[E]() extends Reader[E, E]

def ask[E](using c: Ctx, has: Reader % E :? c.R): c.P[E] = perform[Reader % E, E](Reader.Ask[E]())
/** `reader[A](e)(body)`: every `ask` answered `e` */
def reader[A](using c: Ctx)[E](e: E)(body: c.Body[Reader % E, A, A]): c.P[A] =
  handle(new Answering[c.P, Reader % E, A, A]:
    def ret(a: A): A = a
    def value[X](op: Reader[E, X]): X = op match
      case Reader.Ask() => e)(body)
