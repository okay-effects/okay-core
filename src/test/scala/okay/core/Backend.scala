package okay.core

/** A MONAD of master's `Delimited`, for the tests written once: its run of a program at the top, over `Pure` */
trait Backend:
  def run[A](body: (c: Delimited { type R = Pure; type S = A }) ?=> c.M[Pure, A, A, A]): A

object Backend:
  /** `Cont`: `Effects.At` the context */
  object cont extends Backend:
    def run[A](body: (c: Delimited { type R = Pure; type S = A }) ?=> c.M[Pure, A, A, A]): A =
      Effects.run(body(using summon[Effects.At[Pure, A]]))
