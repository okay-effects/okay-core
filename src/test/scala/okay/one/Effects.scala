package okay.one

import okay.one.handler.*
import okay.one.row.*

/** the effects of the tests */
enum Ask[A]:
  case Get extends Ask[Int]

enum Log[A]:
  case Tell(s: String) extends Log[Unit]

enum Choose[A]:
  case Flip extends Choose[Boolean]
  case Fail extends Choose[Nothing]

enum St[A]:
  case Get extends St[Int]
  case Put(s: Int) extends St[Unit]

enum Throws[A]:
  case Raise(e: String) extends Throws[Nothing]

/** their handlers, one of each kind and more — each over any row `T` outside it */
object Handlers:
  /** each `Get` answered `n` in place */
  def reader[T <: Row, A](n: Int) = new Answering[Ask, T, A, A]:
    def ret(a: A) = Cont.pure(a)
    def value[X](op: Ask[X]): X = op match { case Ask.Get => n }

  /** the told, a list: a state */
  def log[T <: Row, A] = new Stateful[Log, T, List[String], A, (List[String], A)]:
    def initial = Nil
    def ret(s: List[String], a: A) = Cont.pure(s.reverse -> a)
    def value[X](s: List[String], op: Log[X]): X = op match { case Log.Tell(_) => () }
    def next[X](s: List[String], op: Log[X]) = op match { case Log.Tell(m) => m :: s }

  def state[T <: Row, A](s0: Int) = new Stateful[St, T, Int, A, (Int, A)]:
    def initial = s0
    def ret(s: Int, a: A) = Cont.pure(s -> a)
    def value[X](s: Int, op: St[X]): X = op match
      case St.Get => s
      case St.Put(_) => ()
    def next[X](s: Int, op: St[X]) = op match
      case St.Get => s
      case St.Put(s1) => s1

  def attempt[T <: Row, A] = new Aborting[Throws, T, A, Either[String, A]]:
    def ret(a: A) = Cont.pure(Right(a))
    def abort[X](op: Throws[X]) = op match { case Throws.Raise(e) => Cont.pure(Left(e)) }

  /** each path, all of their values */
  def choose[T <: Row, A] = new Deep[Choose, T, A, List[A]]:
    def ret(a: A) = Cont.pure(List(a))
    def apply[X](op: Choose[X], k: X => List[A] ! T) = op match
      case Choose.Flip => for a <- k(true); b <- k(false) yield a ++ b
      case Choose.Fail => Cont.pure(Nil)

  /** each `Get` answered `n`, told first: an operation of the row outside in its clause */
  def askLogged[T <: Row, A](n: Int)(using In[Log, T]) = new Deep[Ask, T, A, A]:
    def ret(a: A) = Cont.pure(a)
    def apply[X](op: Ask[X], k: X => A ! T) = op match
      case Ask.Get => Cont.perform[Log, T, Unit](Log.Tell(s"asked $n")).flatMap(_ => k(n))

  /** each `Get` answered from a state, told: a program of effects of the row outside in its clause */
  def askState[T <: Row, A](using In[St, T], In[Log, T]) = new Deep[Ask, T, A, A]:
    def ret(a: A) = Cont.pure(a)
    def apply[X](op: Ask[X], k: X => A ! T) = op match
      case Ask.Get =>
        val r = Cont.rows[T]
        for s <- r.perform(St.Get); _ <- r.perform(Log.Tell(s"state $s")); a <- k(s) yield a
