package okay.zero

import scala.annotation.tailrec

/**
 * THE MONAD OF shift0 AND $, answers modified — Materzok–Biernacki's, as small as it goes. `Cont0[I, O, A]`: a
 * program of value `A` which, with a continuation answering `I`, answers `O`, `(A => I) => O`. Under a delimiter
 * the answers are programs of the level outside — the levels nested in the answer types.
 *
 *  - `ret $ body` (`Dollar`): the body's value through `ret` to its answer `I`, a program outside; the body's final
 *    answer the delimiter's.
 *  - `shift0(k => e)`: `k` the rest up to the nearest delimiter and the delimiter — `λx. ret $ K[x]`; `e` runs in
 *    the delimiter's place, outside it.
 *
 * Four nodes; `step` runs one to its head with a stack of frames, in constant stack. THE KERNEL, hidden: effects
 * and control are built on it in this package, and nothing outside reaches it — a raw delimiter would catch the
 * requests on their way to the handlers.
 */
private[zero] enum Cont0[I, O, A]:
  case Return[S, A](a: A) extends Cont0[S, S, A]
  case Bind[I, T, O, A, B](m: Cont0[T, O, A], f: A => Cont0[I, T, B]) extends Cont0[I, O, B]
  case Shift0[I1, O1, X1, O, A](f: (A => Cont0[I1, O1, X1]) => O) extends Cont0[Cont0[I1, O1, X1], O, A]
  case Dollar[I, I2, O2, B, A](body: Cont0[I, Cont0[I2, O2, B], A], ret: A => I) extends Cont0[I2, O2, B]

  def flatMap[I2, B](f: A => Cont0[I2, I, B]): Cont0[I2, O, B] = Bind(this, f)
  def map[B](f: A => B): Cont0[I, O, B] = Bind(this, a => Return(f(a)))

/** what a value goes through, innermost first; as a function, the rest */
private[zero] enum Frames[A, I, T, B] extends (A => Cont0[I, T, B]):
  case End[S, A]() extends Frames[A, S, S, A]
  case Then[A, X, I, T1, T, B](f: A => Cont0[T1, T, X], next: Frames[X, I, T1, B]) extends Frames[A, I, T, B]

  def apply(a: A): Cont0[I, T, B] = this match
    case End() => Cont0.Return(a)
    // the rest bound after the first frame — none, the first frame's program itself: an empty stack bound as a
    // frame would be one more each resumption, nested
    case Then(f, next) => next match
      case End() => f(a)
      case _ => Cont0.Bind(f(a), next)

/** a program's head: its value, or a capture with the rest */
private[zero] enum Head[I, O, A]:
  case Done[S, A](a: A) extends Head[S, S, A]
  case Cut[I, I1, O1, X1, O, X, A](f: (X => Cont0[I1, O1, X1]) => O, k: Frames[X, I, Cont0[I1, O1, X1], A]) extends Head[I, O, A]

private[zero] object Cont0:
  def pure[S, A](a: A): Cont0[S, S, A] = Return(a)
  def shift0[I1, O1, X1, O, A](f: (A => Cont0[I1, O1, X1]) => O): Cont0[Cont0[I1, O1, X1], O, A] = Shift0(f)
  def dollar[I, I2, O2, B, A](ret: A => I)(body: Cont0[I, Cont0[I2, O2, B], A]): Cont0[I2, O2, B] = Dollar(body, ret)

  extension [J, I2, O2, B, A](body: Cont0[Cont0[J, J, A], Cont0[I2, O2, B], A])
    /** `pure $ body` */
    def reset0: Cont0[I2, O2, B] = Dollar(body, Return(_))

  /** the program to its head: binds followed, a delimiter entered */
  @tailrec def step[I, T, O, X, A](c: Cont0[T, O, X], k: Frames[X, I, T, A]): Head[I, O, A] = c match
    case Return(a) => k match
      case Frames.End() => Head.Done(a)
      case Frames.Then(f, next) => step(f(a), next)
    case Bind(m, f) => step(m, Frames.Then(f, k))
    case Shift0(f) => Head.Cut(f, k)
    case Dollar(body, ret) => step(delimit(body, ret), k)

  /** `ret $ body` to the program outside: a value through `ret`; a capture's body in the delimiter's place, `k` the
   * rest under the delimiter again */
  private def delimit[I, I2, O2, B, A](body: Cont0[I, Cont0[I2, O2, B], A], ret: A => I): Cont0[I2, O2, B] =
    step(body, Frames.End()) match
      case Head.Done(a) => ret(a)
      case Head.Cut(f, k) => f(x => Dollar(resumed(x, k), ret))

  /** `x` into the rest `k` — LAZILY: a bind the loop takes when it gets to it, so applying `k` runs nothing of the
   * rest (a generator's next step made, not taken); an empty rest, the value itself */
  private def resumed[I, T, X, A](x: X, k: Frames[X, I, T, A]): Cont0[I, T, A] = k match
    case Frames.End() => Return(x)
    case _ => Bind(Return(x), k)

  extension [A](c: Cont0[A, A, A])
    /** a program at the top, its value — the top `identity $` */
    def value: A = step(c, Frames.End()) match
      case Head.Done(a) => a
      case Head.Cut(f, k) => f(x => Dollar(resumed(x, k), identity))
