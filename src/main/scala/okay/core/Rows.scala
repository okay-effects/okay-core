package okay.core

/** a ROW: the effects a program may perform, nominal — `Ask +: Say +: Pure`. Nominal, not a union: a union
 * value cannot be told apart without a cast, and a union type cannot be taken apart by the compiler; a class
 * can, so every walk here is total, no claim, no cast, no runtime test of an operation's class */
sealed trait Row
/** the effect `E` in front of the row `T` */
sealed trait +:[E[_], T <: Row] extends Row
/** the empty row: a program over it performs nothing */
sealed trait Pure extends Row

/** the effect `E` IN the row `R`: a path to it, the compiler builds it — the first, by priority. An operation
 * carries its path; a handler follows it: at `Here`, the operation is the handler's; at `There`, it goes on
 * outside, one effect off the row */
enum In[E[_], R <: Row]:
  case Here[E[_], T <: Row]() extends In[E, E +: T]
  case There[E[_], E2[_], T <: Row](in: In[E, T]) extends In[E, E2 +: T]

/** `E :? R`, the effect `E` in the row `R`: `In` as it is written in a signature, `(using in: Reader % E :? c.Effs)` —
 * a `:`-operator, so `%` and `+:` bind tighter: `Ask :? Ask +: Pure` */
infix type :?[E[_], R <: Row] = In[E, R]

object In extends InLow:
  given here[E[_], T <: Row]: In[E, E +: T] = Here()
sealed trait InLow:
  given there[E[_], E2[_], T <: Row](using in: In[E, T]): In[E, E2 +: T] = In.There(in)

/** an effect of two parameters, its first given: `Reader % Int` is the effect `[X] =>> Reader[Int, X]` — the
 * effect in a row, a path, a handler, by name */
infix type %[F[_, +_], S] = [X] =>> F[S, X]

/** THE ROW `Es` IN THE ROW `R`: a path in `R` for each path in `Es` — the effects a program needs, found in the
 * context's row. Two cases, both total: the last effect, `E0 +: Pure`, where a path can only be `Here` (one past
 * it would be into `Pure`, and there is none — the compiler sees it); and an effect in front of more */
sealed trait Sub[Es <: Row, R <: Row]:
  def apply[E[_]](in: In[E, Es]): In[E, R]
object Sub:
  given last[E0[_], R <: Row](using head: In[E0, R]): Sub[E0 +: Pure, R] = new Sub[E0 +: Pure, R]:
    def apply[E[_]](in: In[E, E0 +: Pure]): In[E, R] = in match
      case In.Here() => head
  given cons[E0[_], E1[_], T <: Row, R <: Row](using head: In[E0, R], tail: Sub[E1 +: T, R]): Sub[E0 +: E1 +: T, R] =
    new Sub[E0 +: E1 +: T, R]:
      def apply[E[_]](in: In[E, E0 +: E1 +: T]): In[E, R] = in match
        case In.Here() => head
        case In.There(out) => tail(out)

/** the effect `E` AMONG THE EFFECTS a program needs, `Es`: the one effect it is, `Only`, or its path in the row of
 * them, `Path`. Which of the two is for the compiler's search to choose — by the kind of `Es`, an effect or a row */
enum Req[E[_], Es <: AnyKind]:
  case Only[E[_]]() extends Req[E, E]
  case Path[E[_], Es <: Row](in: In[E, Es]) extends Req[E, Es]
object Req:
  given only[E[_]]: Req[E, E] = Only()
  given path[E[_], Es <: Row](using in: In[E, Es]): Req[E, Es] = Path(in)

/** THE PATHS of the effects a program needs, `Es` — one effect, or a row of them — in the row `R` */
sealed abstract class Paths[Es <: AnyKind, R <: Row]:
  def find[E[_]](r: Req[E, Es]): In[E, R]
  /** a context of an instance's own making with these paths kept in it, by their kind — an operation then the
   * context's own call, no `Paths` between */
  def kept[C](make: Paths.Keep[R, C]): C & Needs[Es]
object Paths:
  /** how an instance keeps the paths in a context of its own: one effect's, a row's */
  trait Keep[R <: Row, C]:
    def one[E0[_]](head: In[E0, R]): C & Needs[E0]
    def row[Es <: Row](sub: Sub[Es, R]): C & Needs[Es]
  /** one effect, `E0`, at its path */
  final class One[E0[_], R <: Row](val head: In[E0, R]) extends Paths[E0, R]:
    // `Only`: a `Path` is into a row, `E0` is an effect — of another kind, which the checker does not tell apart
    def find[E[_]](r: Req[E, E0]): In[E, R] = (r: @unchecked) match
      case Req.Only() => head
    def kept[C](make: Keep[R, C]): C & Needs[E0] = make.one(head)
  /** a row of them, `Pure` at the end, each at its path */
  final class Of[Es <: Row, R <: Row](val sub: Sub[Es, R]) extends Paths[Es, R]:
    // `Path`: `Only` is for one effect, `Es` is a row — of another kind, as above
    def find[E[_]](r: Req[E, Es]): In[E, R] = (r: @unchecked) match
      case Req.Path(in) => sub(in)
    def kept[C](make: Keep[R, C]): C & Needs[Es] = make.row(sub)
  given one[E0[_], R <: Row](using head: In[E0, R]): Paths[E0, R] = One(head)
  given row[Es <: Row, R <: Row](using sub: Sub[Es, R]): Paths[Es, R] = Of(sub)
