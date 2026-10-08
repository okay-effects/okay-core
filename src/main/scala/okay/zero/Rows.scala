package okay.zero

/** a ROW: the effects of a context, nominal — `Ask +: Say +: Empty`. Control is in any row */
sealed trait Row
sealed trait +:[E[_], T <: Row] extends Row
sealed trait Empty extends Row

/** the effect `E` in the row `R`, a path to it: the compiler builds it, the first by priority */
enum In[E[_], R <: Row]:
  case Here[E[_], T <: Row]() extends In[E, E +: T]
  case There[E[_], E2[_], T <: Row](in: In[E, T]) extends In[E, E2 +: T]
infix type :?[E[_], R <: Row] = In[E, R]
object In extends InLow:
  given here[E[_], T <: Row]: In[E, E +: T] = Here()
sealed trait InLow:
  given there[E[_], E2[_], T <: Row](using in: In[E, T]): In[E, E2 +: T] = In.There(in)

/** an effect of two parameters, its first given: `Reader % Int` is the effect `[X] =>> Reader[Int, X]` */
infix type %[F[_, +_], S] = [X] =>> F[S, X]

/** THE ROW `Es` IN THE ROW `R`: a path in `R` for each path in `Es` — the effects a program needs, found in the
 * context's row. Two cases, both total: the last effect, `E0 +: Empty`, where a path can only be `Here`; and an
 * effect in front of more */
sealed trait Sub[Es <: Row, R <: Row]:
  def apply[E[_]](in: In[E, Es]): In[E, R]
object Sub:
  given last[E0[_], R <: Row](using head: In[E0, R]): Sub[E0 +: Empty, R] = new Sub[E0 +: Empty, R]:
    def apply[E[_]](in: In[E, E0 +: Empty]): In[E, R] = in match
      case In.Here() => head
  given cons[E0[_], E1[_], T <: Row, R <: Row](using head: In[E0, R], tail: Sub[E1 +: T, R]): Sub[E0 +: E1 +: T, R] =
    new Sub[E0 +: E1 +: T, R]:
      def apply[E[_]](in: In[E, E0 +: E1 +: T]): In[E, R] = in match
        case In.Here() => head
        case In.There(out) => tail(out)

/** the effect `E` AMONG THE EFFECTS a program needs, `Es`: the one effect it is, `Only`, or its path in the row of
 * them, `Path` — which, for the compiler's search to choose by the kind of `Es` */
enum Req[E[_], Es <: AnyKind]:
  case Only[E[_]]() extends Req[E, E]
  case Path[E[_], Es <: Row](in: In[E, Es]) extends Req[E, Es]
object Req:
  given only[E[_]]: Req[E, E] = Only()
  given path[E[_], Es <: Row](using in: In[E, Es]): Req[E, Es] = Path(in)

/** THE PATHS of the effects a program needs, `Es` — one effect, or a row of them — in the row `R` */
sealed abstract class Paths[Es <: AnyKind, R <: Row]:
  def find[E[_]](r: Req[E, Es]): In[E, R]
object Paths:
  final class One[E0[_], R <: Row](head: In[E0, R]) extends Paths[E0, R]:
    // `Only`: a `Path` is into a row, `E0` an effect — of another kind, which the checker does not tell apart
    def find[E[_]](r: Req[E, E0]): In[E, R] = (r: @unchecked) match
      case Req.Only() => head
  final class Of[Es <: Row, R <: Row](sub: Sub[Es, R]) extends Paths[Es, R]:
    // `Path`: `Only` is for one effect, `Es` a row — as above
    def find[E[_]](r: Req[E, Es]): In[E, R] = (r: @unchecked) match
      case Req.Path(in) => sub(in)
  given one[E0[_], R <: Row](using head: In[E0, R]): Paths[E0, R] = One(head)
  given row[Es <: Row, R <: Row](using sub: Sub[Es, R]): Paths[Es, R] = Of(sub)
