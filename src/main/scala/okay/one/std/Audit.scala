package okay.one.std

import okay.one.*
import okay.one.Cont.*
import okay.one.handler.*
import okay.one.machine.Machine
import okay.one.row.*

/** THE AUDIT TRAIL as an effect: a record told — what an interceptor writes, handled outside it, so the trail is a
 * value a test reads and a handler sends anywhere */
enum Audit[X]:
  case Record(line: String) extends Audit[Unit]

object Audit:
  def record[R <: Row](line: String)(using In[Audit, R]): Unit ! R = perform[Audit, R, Unit](Record(line))

  /** the trail kept, in order, beside the value */
  def trail[T <: Row, A]: Stateful[Audit, T, List[String], A, (List[String], A)] =
    new Stateful[Audit, T, List[String], A, (List[String], A)]:
      def initial = Nil
      def ret(s: List[String], a: A) = pure((s.reverse, a))
      def value[X](s: List[String], op: Audit[X]): X = op match
        case Record(_) => ()
      def next[X](s: List[String], op: Audit[X]) = op match
        case Record(line) => line :: s

  /** A PLAIN EFFECT INTERCEPTED for a part of the program, `body`, over `E +: T` — `E` in `T` too: its operations
   * to `h`'s clauses, which run outside it, their own operations of `E` reaching the handler further out */
  def intercept[E[_], T <: Row, A, B](h: Handler[E, T, A, B])(body: A ! (E +: T))(using Plain[E], In[E, T]): B ! T =
    Cont.kinds(h, Removed.Head(), body)

  /** A SENSITIVE EFFECT AUDITED for `body`, over `S +: T`: its operations to `i`'s clauses — passed on (`Pass`), changed
   * or refused; `T` has `Audited[S]`, where no program performs `S` */
  def audit[S[_], T <: Row, A, B](i: Interceptor[S, T, A, B])(body: A ! (S +: T))(using Sensitive[S],
      In[Audited[S], T]): B ! T =
    val pass = Pass[S, T](Audited.in(summon[In[Audited[S], T]]))
    val deep = new Deep[S, T, A, B]:
      def ret(a: A): B ! T = i.ret(a)
      def apply[X](op: S[X], k: X => B ! T): B ! T = i[X](op, k, pass)
    Cont.kinds(deep, Removed.Head(), body)

  /** EVERY OPERATION OBSERVED on its way out of `body`, whatever its effect: `o`'s clause given it — told, passed on,
   * refused. Outside, the row `T`: `R` with each sensitive effect audited */
  def observe[R <: Row, T <: Row, A, B](o: Observer[T, A, B])(body: A ! R)(using m: Audited.Marks[R, T]): B ! T =
    m match
      case Audited.Marks.Same() => Observed(o, Machine.Frame.end, body)

  extension [R <: Row, A](p: A ! R)
    /** a SENSITIVE effect handled — under its audit only: `Audited[S]` in the row. `T` unbounded, as `handle`'s */
    def handleAudited[S[_], T, B](h: Handler[S, T & Row, A, B])(using rm: Removed[Audited[S], R, T & Row],
        s: Sensitive[S]): B ! (T & Row) =
      Cont.kinds(h, Audited.removed(rm), p)
