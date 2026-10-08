package okay.zero.std

import okay.zero.*

/** WRITER: a log, every `tell` kept, in order */
enum Writer[W, +A]:
  case Tell[W](w: W) extends Writer[W, Unit]

def tell[W](w: W)(using c: Ctx, has: Writer % W :? c.R): c.P[Unit] = perform[Writer % W, Unit](Writer.Tell(w))
/** `writer[W, A](body)`: the log and the value */
def writer[W, A](using c: Ctx)(body: c.Body[Writer % W, (List[W], A), A]): c.P[(List[W], A)] =
  handle(new Handler[c.P, Writer % W, A, (List[W], A)]:
    def ret(a: A): (List[W], A) = (Nil, a)
    def apply[X](op: Writer[W, X], k: X => c.P[(List[W], A)]): c.P[(List[W], A)] = op match
      case Writer.Tell(w) => k(()).map((log, a) => (w :: log, a)))(body)
