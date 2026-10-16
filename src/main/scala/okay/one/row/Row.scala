package okay.one.row

/** A ROW of effects: none, or an effect `E` before the row `T` — what a program may perform, each to its handler */
sealed trait Row
sealed trait Pure extends Row
sealed trait +:[E[_], T <: Row] extends Row

/** `E` IN THE ROW `R`: its place — what an operation goes to its handler by */
enum In[E[_], R <: Row]:
  case Here[E[_], T <: Row]() extends In[E, E +: T]
  case There[E[_], F[_], T <: Row](in: In[E, T]) extends In[E, F +: T]
  /** A PLACE BY KEY, found AT RUN TIME: the nearest handler in force that says the key `ref` is its own
   * (`Handler.at`), whatever the row — the protocol's place (okay-kernel's: a handler found by identity where the
   * operation runs, a `k` resumed under other handlers seeing those). Its row `R` is phantom: `again` puts the same
   * place in any row. Made by a `Key` (`place`), or of a ref the protocol owns (`okay.core.kernel`) */
  case At[E[_], R <: Row](ref: AnyRef) extends In[E, R]

  /** what the interpreter found at this place last, and where from: kept on the path, one field — read and written
   * whole, so a race costs a look-up again, nothing worse. Not the row's: the interpreter's (`In.Cache`) */
  private[one] var cache: In.Cache[E] | Null = null

object In extends InLow:
  /** an interpreter's memory of a look-up by the path */
  abstract class Cache[E[_]]

  inline given here[E[_], T <: Row]: In[E, E +: T] = Here()

  extension [E[_], R <: Row](at: In.At[E, R])
    /** the same place in the row `R2`: a place by key is in every row — THE TRUSTED STEP of the rows, the row of a
     * place by key phantom (nothing of the machine reads it; `In.At` is told from `Here` and `There` by a type test
     * of the class alone) */
    def again[R2 <: Row]: In.At[E, R2] = at.asInstanceOf[In.At[E, R2]]

  /** a place by key with no handler of it in force: the protocol's error, not a type's */
  def unhandled[E[_], R <: Row](at: In.At[E, R]): Nothing =
    throw IllegalStateException(s"an operation at a place by key with no handler of it in force: ${at.ref}")

  /** TWO PLACES IN ONE ROW THE SAME: then their effects are the same — a row is a list, a place in it names its
   * effect — by the compiler's own reading of the two paths, no cast: `Same[E1, E2]`, or `null`. A walk as long as
   * the paths, the row's length at most */
  @scala.annotation.tailrec
  def same[E1[_], E2[_], R <: Row](a: In[E1, R], b: In[E2, R]): Same[E1, E2] | Null = a match
    case _: In.Here[e1, t1] => b match
      case _: In.Here[e2, t2] => Same.Refl()
      case _ => null
    case t: In.There[e1, x1, t1] => b match
      case u: In.There[e2, x2, t2] => same(t.in, u.in)
      case _ => null
    // a place by key: not a path, never the same as one — nor told the same as another by the rows (a `Key` tells)
    case _: In.At[e1, r1] => null

  /** the effects `E1` and `E2`, the same: an operation of one an operation of the other, a place of one a place of
   * the other */
  enum Same[E1[_], E2[_]]:
    case Refl[E[_]]() extends Same[E, E]
    def apply[X](op: E1[X]): E2[X] = this match
      case Refl() => op
    def in[R <: Row](p: In[E2, R]): In[E1, R] = this match
      case Refl() => p
  object Same:
    private val theRefl: Same[[x] =>> Any, [x] =>> Any] = Refl[[x] =>> Any]()
    /** the one `Refl`, shared — a key found its own, no object made. THE CAST, and why it is sound: `Refl` holds
     * nothing, and it is answered only where a key told the two effects one (`Handler.at`, `Key.at`) */
    def refl[E1[_], E2[_]]: Same[E1, E2] = theRefl.asInstanceOf[Same[E1, E2]]

  /** a place in `Pure`: none there but `null` — or a place by key, no handler of it in force */
  def absurd[E[_]](in: In[E, Pure]): Nothing = in match
    case at: In.At[e, p] => unhandled(at)
    case null => throw MatchError(in)

/** A KEY: the effect `E` named by `ref`, a place for it in any row found AT RUN TIME (`In.At`) — the protocol's
 * way (okay-kernel's: operations and handlers carry a key, interned, one a type; a handler is found by it where the
 * operation runs). One ref, one effect: `at` is THE TRUSTED STEP of the keys, as okay-kernel's `Key.of` — sound as
 * long as a ref is given to one effect; `of` is the place to give it */
final class Key[E[_]] private (val ref: AnyRef):
  /** the place of this key in the row `R`: an operation performed at it goes to the nearest handler of the key */
  def place[R <: Row]: In.At[E, R] = In.At(ref)
  /** the place `in` this key's — then its effect this key's: by the ref, one effect's */
  def at[F[_], R <: Row](in: In.At[F, R]): In.Same[E, F] | Null =
    if in.ref eq ref then In.Same.refl[E, F] else null
  override def toString: String = s"Key($ref)"

object Key:
  /** the key of `E` named by `ref`: one ref to one effect, the caller's word */
  def of[E[_]](ref: AnyRef): Key[E] = Key(ref)
  /** a key no other is */
  def fresh[E[_]](): Key[E] = Key(new AnyRef)

trait InLow:
  given there[E[_], F[_], T <: Row](using in: In[E, T]): In[E, F +: T] = In.There(in)

/** `T` THE ROW `R` WITH `E` REMOVED: `E` the head (`Head`), or past an effect (`Next`) — where a handler of `E` takes
 * its operations from, any place in the row */
enum Removed[E[_], R <: Row, T <: Row]:
  case Head[E[_], T <: Row]() extends Removed[E, E +: T, T]
  case Next[E[_], G[_], R <: Row, T <: Row](rm: Removed[E, R, T]) extends Removed[E, G +: R, G +: T]
  /** none taken: the row as it is, `E` before it — a program over `T` seen over `E +: T` (`widen`). Not a given */
  case Absent[E[_], T <: Row]() extends Removed[E, T, T]
  /** `E` taken from its place `at` and LEFT THERE TOO: a handler of `E` here, the row outside it `R` as it is — so
   * the handler's own clauses' operations of `E` go to the handler of `E` outside, at `at`: an interceptor
   * (`local`, `handleWith`). Not a given: by the place, `Removed.again(at)` */
  case Again[E[_], R <: Row](at: In[E, R]) extends Removed[E, R, R]

object Removed extends RemovedLow:
  given head[E[_], T <: Row](using NotControl[E]): Removed[E, E +: T, T] = Head()
  /** a handler of `E` where `E` is at `at`, the row kept: an interceptor's */
  def again[E[_], R <: Row](at: In[E, R]): Removed[E, R, R] = Again(at)

  /** a place in `R` SEEN in `E +: T`, `E` taken out of `R` by `rm` and put first: a handler of `E` at any place in the
   * row is the handler at its head, over the row seen so. `Head`: the place itself, nothing made */
  def relocate[F[_], E[_], R <: Row, T <: Row](in: In[F, R], rm: Removed[E, R, T]): In[F, E +: T] = in match
    // a place by key: the same in every row
    case at: In.At[f, r] => at.again
    case _ => relocated(in, rm)

  private def relocated[F[_], E[_], R <: Row, T <: Row](in: In[F, R], rm: Removed[E, R, T]): In[F, E +: T] = rm match
    case Removed.Head() => in
    case _: Removed.Absent[e, t] => In.There(in)
    // the interceptor's own effect, by the place: the head; any other past it, its place as it was
    case ag: Removed.Again[e, r] => In.same(in, ag.at) match
      case null => In.There(in)
      case s: In.Same[F, e] => s.in(In.Here[e, r]())
    case nx: Removed.Next[e, g, r1, t1] => in match
      case _: In.Here[f, t] => In.There[f, e, g +: t1](In.Here[f, t1]())
      case th: In.There[f, g2, r2] => relocate(th.in, nx.rm) match
        case In.Here() => In.Here[f, g +: t1]()
        case In.There(in) => In.There[f, e, g +: t1](In.There[f, g, t1](in))
        case at: In.At[f2, r3] => at.again
      case at: In.At[f, r] => at.again

trait RemovedLow:
  /** never past a `Control`: a handler takes its effect from inside its `$` */
  given next[E[_], G[_], R <: Row, T <: Row](using nc: NotControl[G], rm: Removed[E, R, T]): Removed[E, G +: R, G +: T] =
    Removed.Next(rm)

/** an operation of `F` LOCATED by a handler of `E` removed from its row: its own (`F` is `E`), or another's, its place
 * in the row outside the handler */
enum Located[F[_], E[_], T <: Row]:
  case Ours[E[_], T <: Row]() extends Located[E, E, T]
  case Theirs[F[_], E[_], T <: Row](in: In[F, T]) extends Located[F, E, T]

object Located:
  /** `in` by `rm`: the handler's, or its place in the row outside it */
  def apply[F[_], E[_], R <: Row, T <: Row](in: In[F, R], rm: Removed[E, R, T]): Located[F, E, T] = in match
    case at: In.At[f, r] => Theirs(at.again)
    case _ => located(in, rm)

  private def located[F[_], E[_], R <: Row, T <: Row](in: In[F, R], rm: Removed[E, R, T]): Located[F, E, T] = rm match
    case _: Removed.Absent[e, t] => Theirs(in)
    case ag: Removed.Again[e, r] => In.same(in, ag.at) match
      case null => Theirs(in)
      case s: In.Same[F, e] => s match { case In.Same.Refl() => Ours() }
    case Removed.Head() => in match
      case In.Here() => Ours()
      case In.There(in) => Theirs(in)
      case at: In.At[f, r] => Theirs(at.again)
    case nx: Removed.Next[e, g, r1, t1] => in match
      case _: In.Here[f, t] => Theirs[f, E, f +: t1](In.Here[f, t1]())
      case th: In.There[f, g2, r2] => Located(th.in, nx.rm) match
        case Ours() => Ours()
        case Theirs(in) => Theirs[f, E, g +: t1](In.There[f, g, t1](in))
      case at: In.At[f, r] => Theirs(at.again)

/** code over a row NEEDING the effect `E` in it: `def p[R <: Row: Has[Ask]: Has[Log]]: Int ! R` */
type Has[E[_]] = [R <: Row] =>> In[E, R]

/** `E :? R`: the effect `E` in the row `R`, `In[E, R]` as a signature writes it */
infix type :?[E[_], R <: Row] = In[E, R]
