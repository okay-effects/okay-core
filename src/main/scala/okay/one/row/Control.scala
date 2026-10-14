package okay.one.row

import scala.util.NotGiven

/** CONTROL: the effect of a `$`, `T` the row outside it — in the row as `Control[T] +: T`. No operation of
 * its own: a shift0 is a node, it moves the answer, which an operation cannot; its handler the `$`. The answer types
 * `I`, `O` of a program are its state — the nearest `$`'s in the row; of a row with none, inert. An operation goes
 * out past it as past any effect, by `In.There`; a handler takes its effect from before it, never past it
 * (`Removed`); a handler of it there is none (`NotControl`) */
enum ControlOp[T <: Row, X]:
  /** none: a case with no value */
  case Never[T <: Row](n: Nothing) extends ControlOp[T, Nothing]
type Control[T <: Row] = [x] =>> ControlOp[T, x]

object ControlOp:
  /** an operation of `Control`: none */
  def absurd[T <: Row, X](op: ControlOp[T, X]): Nothing = op match
    case ControlOp.Never(n) => n

/** `E` not `Control`: what a handler may be of */
type NotControl[E[_]] = NotGiven[E[Any] <:< ControlOp[?, Any]]

/** THE NEAREST `$` IN THE ROW `R`, `T` the row outside it: a shift0's path, past the handlers before the `$` — each
 * into its `k` — never past a `$`: a `$`'s answer is its own. Found as `In` is; given only */
enum Nearest[T <: Row, R <: Row]:
  case Here[T <: Row]() extends Nearest[T, Control[T] +: T]
  case There[E[_], T <: Row, R <: Row] private[one] (n: Nearest[T, R]) extends Nearest[T, E +: R]

object Nearest extends NearestLow:
  inline given here[T <: Row]: Nearest[T, Control[T] +: T] = Here()

  /** none: a `$` where a handler is, or a path past a `$` — which the givens refuse, and `There` is theirs only */
  def absurd[T <: Row, R <: Row](n: Nearest[T, R]): Nothing = throw MatchError(n)

  /** the path SEEN in `E +: T2`, `E` taken out of `R` by `rm` and put first — as `Removed.relocate` for `In` */
  def relocate[T <: Row, E[_], R <: Row, T2 <: Row](at: Nearest[T, R], rm: Removed[E, R, T2]): Nearest[T, E +: T2] =
    rm match
      case Removed.Head() => at
      case _: Removed.Absent[e, t] => There(at)
      case nx: Removed.Next[e, g, r1, t1] => at match
        case There(at1) => relocate(at1, nx.rm) match
          case There(n1) => There[e, T, g +: t1](There[g, T, t1](n1))
          case _: Here[t3] => absurd(at)
        case _: Here[t3] => absurd(at)

trait NearestLow:
  given there[E[_], T <: Row, R <: Row](using nc: NotControl[E], n: Nearest[T, R]): Nearest[T, E +: R] = Nearest.There(n)
