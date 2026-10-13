package okay.std

import okay.core.*

/** WRITER: a log, every `tell` kept, in order — a state of the kernel's, each path its own */
export okay.one.std.Writer

def tell[W](w: W): Op[Writer % W, Unit] = perform(Writer.Tell(w))
/** `writer(body)`: the log and the value */
def writer[W, A, T <: Row](body: Under[Writer % W, T, A]): Eff[T, (List[W], A)] =
  handleIn(Writer.collect[W, T, A])(body)
