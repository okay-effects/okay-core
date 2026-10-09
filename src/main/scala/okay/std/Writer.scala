package okay.std

import okay.core.*

/** WRITER: a log, every `tell` kept, in order */
enum Writer[W, +A]:
  case Tell[W](w: W) extends Writer[W, Unit]

def tell[W](w: W)(using c: Effects, has: Writer % W :? c.Effs): c.Prog[Unit] = perform[Writer % W, Unit](Writer.Tell(w))
/** `writer[W, A](body)`: the log and the value */
def writer[W, A](using c: Effects)(body: c.Body[Writer % W, (List[W], A), A])
    : c.Prog[(List[W], A)] =
  type Ans = (List[W], A)
  Effects.handle(new Handler[c.Prog, Writer % W, A, Ans]:
    def ret(a: A): Ans = (Nil, a)
    def apply[X](op: Writer[W, X], k: X => c.Prog[Ans]): c.Prog[Ans] = op match
      case Writer.Tell(w) => k(()).map((log, a) => (w :: log, a)))(body)
