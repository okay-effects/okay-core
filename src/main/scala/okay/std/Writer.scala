package okay.std

import okay.core.*

/** WRITER: a log, every `tell` kept, in order */
enum Writer[W, +A]:
  case Tell[W](w: W) extends Writer[W, Unit]

def tell[W](w: W)(using c: Effects, has: Writer % W :? c.R): c.M[c.R, c.S, c.S, Unit] = perform[Writer % W, Unit](Writer.Tell(w))
/** `writer[W, A](body)`: the log and the value */
def writer[W, A](using c: Effects)(body: c.Under[Writer % W +: c.R, (List[W], A)] ?=> c.M[Writer % W +: c.R, (List[W], A), (List[W], A), A])
    : c.M[c.R, c.S, c.S, (List[W], A)] =
  type Ans = (List[W], A)
  Effects.handle(new Handler[c.M, Writer % W, c.R, c.S, A, Ans]:
    def ret(a: A): Ans = (Nil, a)
    def apply[X](op: Writer[W, X], k: X => c.M[c.R, c.S, c.S, Ans]): c.M[c.R, c.S, c.S, Ans] = op match
      case Writer.Tell(w) => k(()).map((log, a) => (w :: log, a)))(body)
