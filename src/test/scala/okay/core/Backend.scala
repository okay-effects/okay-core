package okay.core

import okay.cont.{Cont, Free, At}

/** A MONAD of master's `Effects`, for the tests written once: its run of a program at the top, over `Pure` */
trait Backend:
  def run[A](body: (c: Effects { type Effs = Pure; type S = A }) ?=> c.Prog[A]): A

object Backend:
  /** `Cont`: `Effects.At` the context */
  object cont extends Backend:
    def run[A](body: (c: Effects { type Effs = Pure; type S = A }) ?=> c.Prog[A]): A =
      okay.cont.run(body(using summon[okay.cont.At[Pure, A]]))
