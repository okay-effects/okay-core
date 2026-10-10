package okay.std

import okay.core.*

/** THE STANDARD EFFECTS AS `Program`s: the row in the type, each built in the context it runs in — for code written
 * over `F[_]` (`Program.Of[R]`), for okay's shape, and for Scala 2. A handler takes its effect off WHEREVER it is in
 * the body's row (`Removed`, okay's): the body built in a view of the handler's context (`View`), its paths
 * translated; the result over the row without it, `Out`. Each the context's handler, made one of programs by
 * `Program.handled` (an interceptor by `Program.within`): an effect written once, in context */
object Programs:
  def ask[R <: Row, E](using in: Reader % E :? R): Program[R, E] = Program.perform[Reader % E, R, E](Reader.Ask[E]())
  /** every `ask` of `body` answered `e`, the reader wherever it is in its row */
  def reader[E, R <: Row, A](e: E)(body: Program[R, A])(using rm: Removed[Reader % E, R]): Program[rm.Out, A] =
    Program.handled[Reader % E, R, A, A](body)(c => b => okay.std.reader[A](using c)(e)(b))
  /** every `ask` of `fa` answered `f` of the value outside: `fa` built again under an interceptor */
  def local[R <: Row, E, A](f: E => E)(fa: Program[R, A])(using in: Reader % E :? R): Program[R, A] =
    Program.within[R, A, A](fa)(c => b => okay.std.local[E, A](f)(using c, in)(b))

  def get[R <: Row, S](using in: State % S :? R): Program[R, S] = Program.perform[State % S, R, S](State.Get[S]())
  def put[R <: Row, S](s: S)(using in: State % S :? R): Program[R, Unit] = Program.perform[State % S, R, Unit](State.Put(s))
  /** the last state and the value, the state wherever it is in the body's row */
  def state[S, R <: Row, A](s0: S)(body: Program[R, A])(using rm: Removed[State % S, R]): Program[rm.Out, (S, A)] =
    Program.handled[State % S, R, A, (S, A)](body)(c => b => okay.std.state[A](using c)(s0)(b))

  def tell[R <: Row, W](w: W)(using in: Writer % W :? R): Program[R, Unit] = Program.perform[Writer % W, R, Unit](Writer.Tell(w))
  /** `writer[W](body)`: the log and the value, the writer wherever it is in the body's row */
  def writer[W]: WriterOf[W] = WriterOf()
  final class WriterOf[W] private[Programs] ():
    def apply[R <: Row, A](body: Program[R, A])(using rm: Removed[Writer % W, R]): Program[rm.Out, (List[W], A)] =
      Program.handled[Writer % W, R, A, (List[W], A)](body)(c => b => okay.std.writer[W, A](using c)(b))

  def raise[R <: Row, E, A](e: E)(using in: Throws % E :? R): Program[R, A] = Program.perform[Throws % E, R, A](Throws.Raise(e))
  /** `throws[E](body)`: the value as `Right`, or the first `raise` as `Left` — the throws wherever it is in the row */
  def throws[E]: ThrowsOf[E] = ThrowsOf()
  final class ThrowsOf[E] private[Programs] ():
    def apply[R <: Row, A](body: Program[R, A])(using rm: Removed[Throws % E, R]): Program[rm.Out, Either[E, A]] =
      Program.handled[Throws % E, R, A, Either[E, A]](body)(c => b => okay.std.throws[E, A](using c)(b))
  /** a `raise` of `fa` answered `f` of the error: `fa` built again under an interceptor */
  def handleWith[R <: Row, E, A](fa: Program[R, A])(f: E => Program[R, A])(using in: Throws % E :? R): Program[R, A] =
    Program.within[R, A, A](fa)(c => b => okay.std.handleWith[E, A](using c, in)(b)(e => f(e).on(c)))
