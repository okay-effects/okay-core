package okay.one.row

import scala.annotation.implicitNotFound
import scala.util.NotGiven

/** A SENSITIVE EFFECT: handled only under its audit — given by the effect's author, `given Sensitive[Pay] =
 * Sensitive()`. An audit of it (`Audit.audit`, an interceptor) leaves `Audited[Pay]` in the row outside: no program
 * there performs `Pay`, and only `handleAudited` handles it */
final class Sensitive[E[_]]

object Sensitive:
  def apply[E[_]](): Sensitive[E] = new Sensitive[E]

/** A PLAIN EFFECT: any handler's — what `handle` asks, so a sensitive one is not handled by it */
@implicitNotFound("a sensitive effect is handled only under its audit: audit it first (`Audit.audit` with an " +
  "interceptor of it, or `Audit.observe`), then handle it with `handleAudited`")
final class Plain[E[_]] private ()

object Plain:
  given plain[E[_]](using NotGiven[Sensitive[E]]): Plain[E] = new Plain[E]

/** `E` AUDITED: in the row outside an audit of it — no `In[E, R]` there, so no program performs it; the same
 * operations at run time, so the paths and the handlers are the same */
type Audited[E[_]] = [x] =>> Audited.Op[E, x]

object Audited:
  /** an operation of `E` audited: one of `E`, known so only here */
  opaque type Op[E[_], X] = E[X]

  /** the place of `E` audited: its own — for the audit's way on, and its handler */
  private[one] def in[E[_], R <: Row](in: In[Audited[E], R]): In[E, R] = in

  /** `E` audited removed: `E` removed — the handler of `E`, under its audit */
  private[one] def removed[E[_], R <: Row, T <: Row](rm: Removed[Audited[E], R, T]): Removed[E, R, T] = rm

  /** `T`, THE ROW `R` WITH EACH SENSITIVE EFFECT AUDITED — the row outside an observer, which sees them all; the same
   * row at run time, `Same` its only case */
  enum Marks[R <: Row, T <: Row]:
    case Same[R <: Row] private[one] () extends Marks[R, R]

  object Marks extends MarksLow:
    given pure: Marks[Pure, Pure] = Same()
    given sensitive[E[_], R <: Row, T <: Row](using Sensitive[E], Marks[R, T]): Marks[E +: R, Audited[E] +: T] =
      summon[Marks[R, T]] match
        case Same() => Same[E +: R]()

  trait MarksLow:
    given plain[E[_], R <: Row, T <: Row](using NotGiven[Sensitive[E]], Marks[R, T]): Marks[E +: R, E +: T] =
      summon[Marks[R, T]] match
        case Marks.Same() => Marks.Same[E +: R]()
