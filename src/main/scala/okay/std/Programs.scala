package okay.std

import okay.core.*

/** THE STANDARD EFFECTS AS `Program`s: the row in the type, each built in the context it runs in — for code written
 * over `F[_]` (`Program.Of[R]`), and for Scala 2. The same handlers as the context's, called where the program is
 * built */
object Programs:
  def ask[R <: Row, E](using in: Reader % E :? R): Program[R, E] = Program.perform[Reader % E, R, E](Reader.Ask[E]())
  def reader[R <: Row, E, A](e: E)(body: Program[Reader % E +: R, A]): Program[R, A] = new Program[R, A]:
    def on(c: Effects { type Effs = R }): c.Prog[A] = okay.std.reader[A](using c)(e)(h ?=> body.on(h))
  /** every `ask` of `fa` answered `f` of the value outside: `fa` built again under an interceptor */
  def local[R <: Row, E, A](f: E => E)(fa: Program[R, A])(using in: Reader % E :? R): Program[R, A] = new Program[R, A]:
    def on(c: Effects { type Effs = R }): c.Prog[A] = okay.std.local[E, A](f)(using c, in)(r ?=> fa.on(r))

  def get[R <: Row, S](using in: State % S :? R): Program[R, S] = Program.perform[State % S, R, S](State.Get[S]())
  def put[R <: Row, S](s: S)(using in: State % S :? R): Program[R, Unit] = Program.perform[State % S, R, Unit](State.Put(s))
  def state[R <: Row, S, A](s0: S)(body: Program[State % S +: R, A]): Program[R, (S, A)] = new Program[R, (S, A)]:
    def on(c: Effects { type Effs = R }): c.Prog[(S, A)] = okay.std.state[A](using c)(s0)(h ?=> body.on(h))

  def tell[R <: Row, W](w: W)(using in: Writer % W :? R): Program[R, Unit] = Program.perform[Writer % W, R, Unit](Writer.Tell(w))
  def writer[R <: Row, W, A](body: Program[Writer % W +: R, A]): Program[R, (List[W], A)] = new Program[R, (List[W], A)]:
    def on(c: Effects { type Effs = R }): c.Prog[(List[W], A)] = okay.std.writer[W, A](using c)(h ?=> body.on(h))

  def raise[R <: Row, E, A](e: E)(using in: Throws % E :? R): Program[R, A] = Program.perform[Throws % E, R, A](Throws.Raise(e))
  def throws[R <: Row, E, A](body: Program[Throws % E +: R, A]): Program[R, Either[E, A]] = new Program[R, Either[E, A]]:
    def on(c: Effects { type Effs = R }): c.Prog[Either[E, A]] = okay.std.throws[E, A](using c)(h ?=> body.on(h))
  /** a `raise` of `fa` answered `f` of the error: `fa` built again under an interceptor */
  def handleWith[R <: Row, E, A](fa: Program[R, A])(f: E => Program[R, A])(using in: Throws % E :? R): Program[R, A] = new Program[R, A]:
    def on(c: Effects { type Effs = R }): c.Prog[A] = okay.std.handleWith[E, A](using c, in)(r ?=> fa.on(r))(e => f(e).on(c))
