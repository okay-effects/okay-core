package okay.core

/** THE CONTEXT a program is written in: its monad, an indexed one — `M[R, I, O, A]` a program over the row `R` of
 * value `A` which, with a continuation answering `I`, answers `O` (Atkey's parameterised monad, the answer moved from
 * `O` to `I`) — the row `Effs` here and the answer `S` at this point: what an operation and a `shift` take their types
 * from. The context IS the instance: a program asks for one, `(using c: Effects)`, and is a value of `c.Prog`. A
 * delimiter gives its body one of its own: a handler's has the handler's effect in front of what the handler
 * leaves, `E +: c.Effs`, so the row is built by the handlers, outside in, and never named. The nearest context wins —
 * a nested scope's given is preferred. Its delimiters' bodies are in the monad ONE LEVEL IN, `Within` (a reset's, an
 * interceptor's, over this row) and `Handling` (a handler's, its effect in front): `Cont`'s the same, `M`; a monad
 * whose answers are programs of the level outside (`okay.zero`'s) one level deeper */
abstract class Context:
  /** a program here, over the row `Effs`: of value `A`, with a continuation answering `I`, answering `O` */
  type M[I, O, A]
  type Effs <: Row
  type S
  /** the monad of the body of a delimiter here over this row — a reset's, an interceptor's: one level in */
  type Within[I, O, A]
  /** a program here, the answer kept: what most programs are */
  type Prog[A] = M[S, S, A]
  def pure[S2, A](a: A): M[S2, S2, A]
  /** `delay(m)`: the program `m`, built when it is run — a tail call */
  def delay[I, O, A](m: => M[I, O, A]): M[I, O, A]
  extension [I, O, A](m: M[I, O, A])
    def flatMap[I2, B](f: A => M[I2, I, B]): M[I2, O, B]
    def map[B](f: A => B): M[I, O, B]
