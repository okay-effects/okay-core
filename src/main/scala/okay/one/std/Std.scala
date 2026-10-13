package okay.one.std

import okay.one.*
import okay.one.Cont0.{perform, pure}
import okay.one.handler.*
import okay.one.row.*

/** an effect of two parameters, its first given: `State % Int`, an effect of one */
infix type %[F[_, _], V] = [x] =>> F[V, x]

/** READING a value of `V` */
enum Reader[V, X]:
  case Ask[V]() extends Reader[V, V]

object Reader:
  def ask[V, R <: Row](using In[Reader % V, R]): V ! R = perform[Reader % V, R, V](Ask())

  /** each `ask` answered `v` */
  def apply[V, T <: Row, A](v: V): Answering[Reader % V, T, A, A] = new Answering[Reader % V, T, A, A]:
    def ret(a: A) = pure(a)
    def value[X](op: Reader[V, X]): X = op match { case _: Ask[w] => v }

/** a STATE of `S` */
enum State[S, X]:
  case Get[S]() extends State[S, S]
  case Put[S](s: S) extends State[S, Unit]

object State:
  def get[S, R <: Row](using In[State % S, R]): S ! R = perform[State % S, R, S](Get())
  def put[S, R <: Row](s: S)(using In[State % S, R]): Unit ! R = perform[State % S, R, Unit](Put(s))
  def modify[S, R <: Row](f: S => S)(using In[State % S, R]): Unit ! R = get[S, R].flatMap(s => put[S, R](f(s)))

  /** from `s0`: the last state and the value. Each `k` taken past it with the state as it was then */
  def apply[S, T <: Row, A](s0: S): Stateful[State % S, T, S, A, (S, A)] = new Stateful[State % S, T, S, A, (S, A)]:
    def initial = s0
    def ret(s: S, a: A) = pure((s, a))
    def value[X](s: S, op: State[S, X]): X = op match
      case _: Get[s] => s
      case _: Put[s] => ()
    def next[X](s: S, op: State[S, X]) = op match
      case _: Get[s] => s
      case p: Put[s] => p.s

/** TELLING values of `W` */
enum Writer[W, X]:
  case Tell[W](w: W) extends Writer[W, Unit]

object Writer:
  def tell[W, R <: Row](w: W)(using In[Writer % W, R]): Unit ! R = perform[Writer % W, R, Unit](Tell(w))

  /** the told, in order, and the value */
  def collect[W, T <: Row, A]: Stateful[Writer % W, T, List[W], A, (List[W], A)] =
    new Stateful[Writer % W, T, List[W], A, (List[W], A)]:
      def initial = Nil
      def ret(s: List[W], a: A) = pure((s.reverse, a))
      def value[X](s: List[W], op: Writer[W, X]): X = op match { case _: Tell[w] => () }
      def next[X](s: List[W], op: Writer[W, X]) = op match { case t: Tell[w] => t.w :: s }

/** an error of `E` RAISED: the rest dropped */
enum Throws[E, X]:
  case Raise[E](e: E) extends Throws[E, Nothing]

object Throws:
  def raise[E, R <: Row](e: E)(using In[Throws % E, R]): Nothing ! R = perform[Throws % E, R, Nothing](Raise(e))

  /** the error raised, or the value */
  def either[E, T <: Row, A]: Aborting[Throws % E, T, A, Either[E, A]] = new Aborting[Throws % E, T, A, Either[E, A]]:
    def ret(a: A) = pure(Right(a))
    def abort[X](op: Throws[E, X]) = op match { case r: Raise[e] => pure(Left(r.e)) }

/** a CHOICE among values: each path taken */
enum Choose[X]:
  case Among[X](xs: Seq[X]) extends Choose[X]

object Choose:
  def among[X, R <: Row](xs: Seq[X])(using In[Choose, R]): X ! R = perform[Choose, R, X](Among(xs))
  def flip[R <: Row](using In[Choose, R]): Boolean ! R = among[Boolean, R](Seq(true, false))
  def fail[X, R <: Row](using In[Choose, R]): X ! R = among[X, R](Nil)

  /** every path's value, in order */
  def all[T <: Row, A]: Deep[Choose, T, A, List[A]] = new Deep[Choose, T, A, List[A]]:
    def ret(a: A) = pure(List(a))
    def apply[X](op: Choose[X], k: X => List[A] ! T) = op match
      case Choose.Among(xs) =>
        def from(i: Int, acc: List[A]): List[A] ! T =
          if i == xs.length then pure(acc.reverse) else k(xs(i)).flatMap(r => from(i + 1, r reverse_::: acc))
        from(0, Nil)
