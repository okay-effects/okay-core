package okay.one.row

/** A ROW of effects: none, or an effect `E` before the row `T` — what a program may perform, each to its handler */
sealed trait Row
sealed trait Pure extends Row
sealed trait +:[E[_], T <: Row] extends Row

/** `E` IN THE ROW `R`: its place — what an operation goes to its handler by */
enum In[E[_], R <: Row]:
  case Here[E[_], T <: Row]() extends In[E, E +: T]
  case There[E[_], F[_], T <: Row](in: In[E, T]) extends In[E, F +: T]

object In extends InLow:
  inline given here[E[_], T <: Row]: In[E, E +: T] = Here()

  /** a place in `Pure`: none there but `null` */
  def absurd[E[_]](in: In[E, Pure]): Nothing = in match
    case null => throw MatchError(in)

trait InLow:
  given there[E[_], F[_], T <: Row](using in: In[E, T]): In[E, F +: T] = In.There(in)

/** `T` THE ROW `R` WITH `E` REMOVED: `E` the head (`Head`), or past an effect (`Next`) — where a handler of `E` takes
 * its operations from, any place in the row */
enum Removed[E[_], R <: Row, T <: Row]:
  case Head[E[_], T <: Row]() extends Removed[E, E +: T, T]
  case Next[E[_], G[_], R <: Row, T <: Row](rm: Removed[E, R, T]) extends Removed[E, G +: R, G +: T]
  /** none taken: the row as it is, `E` before it — a program over `T` seen over `E +: T` (`widen`). Not a given */
  case Absent[E[_], T <: Row]() extends Removed[E, T, T]

object Removed extends RemovedLow:
  given head[E[_], T <: Row]: Removed[E, E +: T, T] = Head()

  /** a place in `R` SEEN in `E +: T`, `E` taken out of `R` by `rm` and put first: a handler of `E` at any place in the
   * row is the handler at its head, over the row seen so. `Head`: the place itself, nothing made */
  def relocate[F[_], E[_], R <: Row, T <: Row](in: In[F, R], rm: Removed[E, R, T]): In[F, E +: T] = rm match
    case Removed.Head() => in
    case _: Removed.Absent[e, t] => In.There(in)
    case nx: Removed.Next[e, g, r1, t1] => in match
      case _: In.Here[f, t] => In.There[f, e, g +: t1](In.Here[f, t1]())
      case th: In.There[f, g2, r2] => relocate(th.in, nx.rm) match
        case In.Here() => In.Here[f, g +: t1]()
        case In.There(in) => In.There[f, e, g +: t1](In.There[f, g, t1](in))

trait RemovedLow:
  given next[E[_], G[_], R <: Row, T <: Row](using rm: Removed[E, R, T]): Removed[E, G +: R, G +: T] = Removed.Next(rm)

/** an operation of `F` LOCATED by a handler of `E` removed from its row: its own (`F` is `E`), or another's, its place
 * in the row outside the handler */
enum Located[F[_], E[_], T <: Row]:
  case Ours[E[_], T <: Row]() extends Located[E, E, T]
  case Theirs[F[_], E[_], T <: Row](in: In[F, T]) extends Located[F, E, T]

object Located:
  /** `in` by `rm`: the handler's, or its place in the row outside it */
  def apply[F[_], E[_], R <: Row, T <: Row](in: In[F, R], rm: Removed[E, R, T]): Located[F, E, T] = rm match
    case _: Removed.Absent[e, t] => Theirs(in)
    case Removed.Head() => in match
      case In.Here() => Ours()
      case In.There(in) => Theirs(in)
    case nx: Removed.Next[e, g, r1, t1] => in match
      case _: In.Here[f, t] => Theirs[f, E, f +: t1](In.Here[f, t1]())
      case th: In.There[f, g2, r2] => Located(th.in, nx.rm) match
        case Ours() => Ours()
        case Theirs(in) => Theirs[f, E, g +: t1](In.There[f, g, t1](in))

/** code over a row NEEDING the effect `E` in it: `def p[R <: Row: Has[Ask]: Has[Log]]: Int ! R` */
type Has[E[_]] = [R <: Row] =>> In[E, R]

/** `E :? R`: the effect `E` in the row `R`, `In[E, R]` as a signature writes it */
infix type :?[E[_], R <: Row] = In[E, R]
