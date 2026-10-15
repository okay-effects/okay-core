package okay.one.machine

import okay.one.*
import okay.one.Cont.*
import okay.one.handler.*
import okay.one.row.*
import okay.one.step.Step
import scala.annotation.tailrec
import scala.util.control.NonFatal

/** THE MACHINE: a CEK machine over the programs of `Cont` — control `c`, the frames `k` of its level, the levels `m`
 * outside: ONE `Meta`, a handler each, or a `$` (`Dollar`, `Reset`), or a program of effects at an answer (`Lifted`);
 * one loop over it. Typed by the answers (Asai–Kameyama's): a frame takes `X` at the answer `I` of the rest to the
 * value `V` at its bottom, the answer `J` there; a level knows its bottom and its final answer `O`.
 *
 * An operation goes to the handler of its place, past `$`s too: answered in place where it is found so, the levels
 * between untouched; else out past each level, each put around its rest — a `Rest`, `k` as data, holding the `$`s it
 * passed, put back on the `Meta` in force where it is resumed. A shift0 goes out the same way, by its path, to its
 * `$`: the handlers between in its `k`. A handler's place is in its type: none past the top but those the top takes */
object Machine:
  /** `p`, no effects left: its value */
  def run[A](p: A ! Pure): A =
    drive(eval(p, Frame.end, Meta.Top(identity[A], (e: Throwable) => throw e, Out.none[A])))

  /** `p` stepped: to its value, or to its first operation of `R` come to the top — given back with its rest */
  def stepped[R <: Row, A](p: A ! R): Step[R, A] =
    drive(eval(p, Frame.end, Meta.Top((a: A) => Step.Done(a),
      (e: Throwable) => if e eq Cancelled then Step.Cancelled() else Step.Failed(e),
      Out.Given[R, Step[R, A]]([E[_], Y] => (op: E[Y], in: In[E, R], resume: Y => Step[R, A],
        fail: Throwable => Step[R, A], cancel: () => Step[R, A]) => Step.Yield(op, in, Step.Resume(resume, fail, cancel))))))

  /** `p` run to its value or its first operation of `R` come to the top — handed out as one object, the rest the
   * machine's own frames and levels: the light form of `stepped` for a driver that pulls (a stream, an iterator) */
  def head[R <: Row, A](p: A ! R): Head[R, A] =
    drive(eval(p, Frame.end, Meta.Top[R, A, Head[R, A]]((a: A) => Head.Done(a), (e: Throwable) => throw e, Out.Paused[R, A]())))

  /** A PROGRAM AT ITS HEAD: its value, or an operation of `R` paused with the rest — resumed with the operation's
   * value, or failed where it was; any number of times, each a run of its own */
  sealed abstract class Head[R <: Row, A]
  object Head:
    final case class Done[R <: Row, A](a: A) extends Head[R, A]
    final class Paused[R <: Row, A, E[_], Y, I, O, B, V, J] private[Machine] (val op: E[Y], val in: In[E, R],
        g: Y => Cont[R, I, O, B], gf: Fails[Cont[R, I, O, B]] | Null, k: Frame[R, B, I, V, J],
        top: Meta.Top[R, V, Head[R, A]]) extends Head[R, A]:
      /** the rest with the operation's value */
      def resume(y: Y): Head[R, A] = drive(eval[R, I, O, B, V, J, Head[R, A]](guarded(g(y)), k,
        top.asInstanceOf[Meta[R, V, J, O, Head[R, A]]]))
      /** the rest with an error raised where the operation was: its levels' guards asked */
      def fail(e: Throwable): Head[R, A] = drive(eval[R, I, O, B, V, J, Head[R, A]](failed(gf, e), k,
        top.asInstanceOf[Meta[R, V, J, O, Head[R, A]]]))

  // THE DATA

  /** THE FRAMES of a level: a value of `X` to its bottom, the value `V` there; `I` the answer of the rest from here,
   * `J` the answer at the bottom */
  enum Frame[R <: Row, -X, I, +V, J]:
    case Bind[R <: Row, X, I, T, A, V, J](f: X => Cont[R, I, T, A], rest: Frame[R, A, I, V, J]) extends Frame[R, X, T, V, J]
    case Map[R <: Row, X, I, A, V, J](f: X => A, rest: Frame[R, A, I, V, J]) extends Frame[R, X, I, V, J]
    case End[R <: Row, V, J]() extends Frame[R, V, J, V, J]

  object Frame:
    private val theEnd: End[Pure, Any, Any] = End()
    /** the end of the frames: one shared, it holds nothing */
    def end[R <: Row, V, J]: Frame[R, V, J, V, J] = theEnd.asInstanceOf[Frame[R, V, J, V, J]]

  /** A REST GIVEN AN ERROR: a `k`'s levels put back, the error raised inside them — their finalizers run, their
   * recovers asked; `null`, raised where the rest is */
  trait Fails[+C]:
    def fail(e: Throwable): C

  /** THE REST, `k`, as data: the levels an operation or a shift0 went out past, outermost first, each a level with the
   * frames inside it, the innermost over the rest `inner` — `k(y)` a `Resume`: the machine puts the levels back on
   * the `Meta` where it is resumed, outermost first, then runs `inner(y)` under them. Of each kind of level a case;
   * `I`, `O` the answers of the rest, where it is resumed */
  enum Rest[-Y, T <: Row, I, +O, B] extends (Y => Cont[T, I, O, B]) with Fails[Cont[T, I, O, B]]:
    case Under[Y, E[_], T <: Row, X, I, A, B, N](h: Taking[E, T, A, B], k: Frame[E +: T, X, I, A, N],
        inner: Y => Cont[E +: T, I, N, X], fails: Fails[Cont[E +: T, I, N, X]] | Null, none: N =:= Nothing,
        pending: (B ! T) | Null)
      extends Rest[Y, T, N, N, B]
    case Through[Y, E[_], T <: Row, X, I, A, J, O, B](h: Passing[E, T, A, B], k: Frame[E +: T, X, I, A, J],
        inner: Y => Cont[E +: T, I, O, X], fails: Fails[Cont[E +: T, I, O, X]] | Null) extends Rest[Y, T, J, O, B]
    case UnderState[Y, E[_], T <: Row, S, X, I, A, J, O, B](h: Stateful[E, T, S, A, B], s: S, k: Frame[E +: T, X, I, A, J],
        inner: Y => Cont[E +: T, I, O, X], fails: Fails[Cont[E +: T, I, O, X]] | Null) extends Rest[Y, T, J, O, B]
    case UnderView[Y, E[_], R <: Row, T <: Row, X, I, A, J, O](rm: Removed[E, R, T], k: Frame[R, X, I, A, J],
        inner: Y => Cont[R, I, O, X], fails: Fails[Cont[R, I, O, X]] | Null) extends Rest[Y, E +: T, J, O, A]
    /** a guard's level: the answers through, as `Through` */
    case UnderGuard[Y, T <: Row, X, I, A, J, O, B](g: Guard[T, A, B], k: Frame[T, X, I, A, J],
        inner: Y => Cont[T, I, O, X], fails: Fails[Cont[T, I, O, X]] | Null, none: (J =:= Nothing) | Null,
        resumed: (() => Unit) | Null) extends Rest[Y, T, J, O, B]
    /** an observer's level: no answer, as a handler's taking the rest */
    case UnderObserve[Y, T <: Row, X, I, A, B, N](o: Observer[T, A, B], k: Frame[T, X, I, A, N],
        inner: Y => Cont[T, I, N, X], fails: Fails[Cont[T, I, N, X]] | Null, none: N =:= Nothing) extends Rest[Y, T, N, N, B]
    /** a program of effects' level at an answer: at any */
    case UnderLifted[Y, R <: Row, X, I, A, S](k: Frame[R, X, I, A, Nothing], inner: Y => Cont[R, I, Nothing, X],
        fails: Fails[Cont[R, I, Nothing, X]] | Null) extends Rest[Y, R, S, S, A]
    /** a `$`: its `ret`, the frames inside it; resumed, the `$` put back, its answer run outside it */
    case UnderDollar[Y, T <: Row, X, I, A, J, I2, O2, B](ret: A => J, k: Frame[Control[T] +: T, X, I, A, J],
        inner: Y => Cont[Control[T] +: T, I, Cont[T, I2, O2, B], X],
        fails: Fails[Cont[Control[T] +: T, I, Cont[T, I2, O2, B], X]] | Null) extends Rest[Y, T, I2, O2, B]
    /** a `reset`'s `$`: its answer a value */
    case UnderReset[Y, R <: Row, X, I, S, O, N](k: Frame[Control[R] +: R, X, I, S, S], inner: Y => Cont[Control[R] +: R, I, O, X],
        fails: Fails[Cont[Control[R] +: R, I, O, X]] | Null, none: N =:= Nothing) extends Rest[Y, R, N, N, O]
    def apply(y: Y): Cont[T, I, O, B] = Resume(this, y)
    def fail(e: Throwable): Cont[T, I, O, B] = Fail(this, e)

  /** a mask among a rest's levels */
  @tailrec private def masked(f: Fails[?] | Null): Boolean = f match
    case cn: Rest.UnderGuard[y, t, x, i, a, j, o, b] => cn.g.masked || masked(cn.fails)
    case cn: Rest.Under[y, e, t, x, i, a, b, n] => masked(cn.fails)
    case cn: Rest.Through[y, e, t, x, i, a, j, o, b] => masked(cn.fails)
    case cn: Rest.UnderState[y, e, t, s, x, i, a, j, o, b] => masked(cn.fails)
    case cn: Rest.UnderView[y, e, r, t, x, i, a, j, o] => masked(cn.fails)
    case cn: Rest.UnderLifted[y, r, x, i, a, s] => masked(cn.fails)
    case cn: Rest.UnderDollar[y, t, x, i, a, j, i2, o2, b] => masked(cn.fails)
    case cn: Rest.UnderReset[y, r, x, i, s, o, n] => masked(cn.fails)
    case cn: Rest.UnderObserve[y, t, x, i, a, b, n] => masked(cn.fails)
    case _ => false

  /** a mask left: a cancel asked under it fired, if no mask is left outside it — unwound from outside it */
  private def maskLeft[T <: Row, V, B, J, V2, J2, O, Z](u: Meta.UnderGuard[T, V, B, J, V2, J2, O, Z], v: V, up: J <:< O): Next[Z] =
    if cancelAsked(u.m) then Eff[T, J2, O, V2, V2, J2, Z](Raise(Cancelled), Frame.end, u.m)
    else Eff[T, J, O, B, V2, J2, Z](left(u, v, up), u.k, u.m)

  /** a guard's `ret` at its bottom: a program of effects, as it is where the level has no answer (`none`), lifted
   * where it has one */
  private def left[T <: Row, V, B, J, V2, J2, O, Z](u: Meta.UnderGuard[T, V, B, J, V2, J2, O, Z], v: V, up: J <:< O)
      : Cont[T, J, O, B] = u.none match
    case null => up.liftCo[[x] =>> Cont[T, J, x, B]](Lift[T, J, B](guarded(u.g.ret(v))))
    case none => none.flip.liftCo[[x] =>> Cont[T, x, Nothing, B]](guarded(u.g.ret(v)))

  /** a cancel asked at the top, no mask between: true once — the ask taken */
  @tailrec private def cancelAsked(m: Meta[?, ?, ?, ?, ?]): Boolean = m match
    case t: Meta.Top[r, v, z] =>
      val asked = t.cancelAsked
      t.cancelAsked = false
      asked
    case u: Meta.UnderGuard[t, v, b, j, v2, j2, o, z] => !u.g.masked && cancelAsked(u.m)
    case _ => m.outer match
      case null => false
      case o => cancelAsked(o)

  /** the program a rest's error starts: raised inside its levels, or where it is */
  private def failed[R <: Row, I, O, A](f: Fails[Cont[R, I, O, A]] | Null, e: Throwable): Cont[R, I, O, A] = f match
    case null => Raise(e)
    case f: Fails[Cont[R, I, O, A]] => f.fail(e)

  /** WHAT THE TOP DOES with an operation come to it: none comes (`Pure`); given back with its rest, a step */
  enum Out[R <: Row, Z]:
    case None[Z]() extends Out[Pure, Z]
    case Given[R <: Row, Z](give: [E[_], Y] => (E[Y], In[E, R], Y => Z, Throwable => Z, () => Z) => Z)
        extends Out[R, Z]
    /** the operation handed out as ONE object, its rest the machine's own (`Head.Paused`): no closure a step */
    case Paused[R <: Row, A]() extends Out[R, Head[R, A]]

  object Out:
    private val theNone: None[Any] = None()
    /** one shared: it holds nothing */
    def none[Z]: Out[Pure, Z] = theNone.asInstanceOf[Out[Pure, Z]]

  /** THE LEVELS in force: what takes a level's value `V` at its bottom, at the answer `J` there, then the frames
   * outside it, and the levels outside those; each with the row of the code inside it, `R`, and its final answer `O`.
   * `UnderState`'s state a register */
  sealed abstract class Meta[R <: Row, -V, J, O, +Z]:
    /** the level outside; none at the top */
    def outer: Meta[?, ?, ?, ?, Z] | Null
    /** if an operation passes this level with its place unchanged (a guard's, a lift's): the first level outside
     * that it does not pass — a run of such levels skipped at once, walked when each is made and not by every look-up
     * (frames nested n deep, each a guard: an operation found in O(1), not O(n)) */
    def passed: Meta[R, ?, ?, ?, Z] | Null = null
  /** the level a passed one hands a look-up to: `m`, or past it if it is passed too — its own skip, so O(1) */
  private def passedFrom[R <: Row, Z](m: Meta[R, ?, ?, ?, Z]): Meta[R, ?, ?, ?, Z] = m.passed match
    case null => m
    case o => o
  object Meta:
    final class Top[R <: Row, V, Z](val k: V => Z, val fail: Throwable => Z, val out: Out[R, Z])
        extends Meta[R, V, Nothing, Nothing, Z]:
      /** a cancel asked under a mask: fired when the program leaves its last */
      var cancelAsked = false
      def outer = null
    /** a handler taking the rest: no answer inside, nor of its rest — `N`, `Nothing` by `none`: a name for it, so
     * that the levels with an answer and those with none are told apart by a type test, not by `Nothing` */
    final class Under[E[_], T <: Row, V, B, N, V2, J2, O2, Z](val h: Taking[E, T, V, B], val k: Frame[T, B, N, V2, J2],
        val m: Meta[T, V2, J2, O2, Z], val none: N =:= Nothing, pending0: (B ! T) | Null) extends Meta[E +: T, V, N, N, Z]:
      /** an abort's program, to run here when the levels inside are left — their finalizers run first. The abort is
       * known by it: a level taken into a `k` meanwhile (a finalizer's operation going out past it) and put back is a
       * level made again, the program with it */
      var pending: (B ! T) | Null = pending0
      def outer = m
    /** a handler leaving the rest: the answers through it */
    final class Through[E[_], T <: Row, V, B, J, V2, J2, O, Z](val h: Passing[E, T, V, B], val k: Frame[T, B, J, V2, J2],
        val m: Meta[T, V2, J2, O, Z]) extends Meta[E +: T, V, J, O, Z]:
      def outer = m
    final class UnderState[E[_], T <: Row, S, V, B, J, V2, J2, O, Z](val h: Stateful[E, T, S, V, B], var s: S,
        val k: Frame[T, B, J, V2, J2], val m: Meta[T, V2, J2, O, Z]) extends Meta[E +: T, V, J, O, Z]:
      def outer = m
    final class UnderView[E[_], R <: Row, T <: Row, V, J, V2, J2, O, Z](val rm: Removed[E, R, T],
        val k: Frame[E +: T, V, J, V2, J2], val m: Meta[E +: T, V2, J2, O, Z]) extends Meta[R, V, J, O, Z]:
      def outer = m
    /** a guard's level: the row the same in it, the answers through as `Through`'s; `none`, no answer at its
     * bottom, for a `Recovering` guard's */
    final class UnderGuard[T <: Row, V, B, J, V2, J2, O, Z](val g: Guard[T, V, B], val k: Frame[T, B, J, V2, J2],
        val m: Meta[T, V2, J2, O, Z], val none: (J =:= Nothing) | Null) extends Meta[T, V, J, O, Z]:
      def outer = m
      override val passed: Meta[T, ?, ?, ?, Z] = passedFrom(m)
    /** an observer's level: the row the same in it, no answer (it takes the rest); every operation on its way out stops
     * here — not passed by `level`, so it goes out by `outE` */
    final class UnderObserve[T <: Row, V, B, N, V2, J2, O2, Z](val o: Observer[T, V, B], val k: Frame[T, B, N, V2, J2],
        val m: Meta[T, V2, J2, O2, Z], val none: N =:= Nothing) extends Meta[T, V, N, N, Z]:
      def outer = m
    /** a program of effects at the answer `S`: its value to the frames outside it */
    final class Lifted[R <: Row, A, S, V, J, O, Z](val k: Frame[R, A, S, V, J], val m: Meta[R, V, J, O, Z], val up: S <:< O)
        extends Meta[R, A, Nothing, Nothing, Z]:
      def outer = m
      override val passed: Meta[R, ?, ?, ?, Z] = passedFrom(m)
    /** a `$`: its answer, a program over the row outside, run by the frames there */
    final class Dollar[T <: Row, A, J, I2, O2, B, V2, J2, Z](val ret: A => J, val k: Frame[T, B, I2, V2, J2],
        val m: Meta[T, V2, J2, O2, Z]) extends Meta[Control[T] +: T, A, J, Cont[T, I2, O2, B], Z]:
      def outer = m
    /** the `$` of a `reset`: its answer a value, to the frames of effects outside it */
    final class Reset[R <: Row, S, O, N, V2, J2, O2, Z](val k: Frame[R, O, N, V2, J2], val m: Meta[R, V2, J2, O2, Z],
        val none: N =:= Nothing) extends Meta[Control[R] +: R, S, S, O, Z]:
      def outer = m

  // THE STATES of the loop

  /** what a step of the loop hands on: the next, the way out of an operation, or the answer */
  sealed abstract class Next[+Z]
  final class Eff[R <: Row, I, O, A, V, J, Z](val c: Cont[R, I, O, A], val k: Frame[R, A, I, V, J],
      val m: Meta[R, V, J, O, Z]) extends Next[Z]
  /** on the way out to a handler taking `k` */
  final class Way[E[_], Y, R <: Row, I, O, A, V, J, Z](val op: E[Y], val in: In[E, R], val g: Y => Cont[R, I, O, A],
      val gf: Fails[Cont[R, I, O, A]] | Null, val k: Frame[R, A, I, V, J], val m: Meta[R, V, J, O, Z]) extends Next[Z]
  final class Done[Z](val z: Z) extends Next[Z]

  /** the loop, to the answer */
  @tailrec def drive[Z](n: Next[Z]): Z = n match
    case e: Eff[r, i, o, a, v, j, z] => drive(eval(e.c, e.k, e.m))
    case w: Way[e, y, r, i, o, a, v, j, z] => drive(outE(w.op, w.in, w.g, w.gf, w.k, w.m))
    case d: Done[z] => d.z

  /** A CALL OF USER CODE: its program, or `Raise` of what it threw — the `try` at the call, never around the loop */
  private inline def guarded[R <: Row, I, O, A](inline c: Cont[R, I, O, A]): Cont[R, I, O, A] =
    try c catch case e: Throwable => raised(e)

  /** A WAY OUT not an error: an abort to a handler's level, a cancel — unwound as one, no guard recovering it.
   * Never thrown: a value of the machine's */
  sealed abstract class Exit extends Throwable(null, null, false, false)
  /** an abort: its program `pending` at a handler's level, run there when the levels inside it are left */
  final class AbortTo(val pending: AnyRef) extends Exit
  /** a cancel, from a step */
  object Cancelled extends Exit

  /** WHAT THE MACHINE RAISES — unwound through the levels, each guard asked — and not thrown on as fatal: what
   * `NonFatal` lets through, and an INTERRUPT. An interrupt is the cancel of the thread running the program (a fiber's,
   * on a blocking scheduler: a park or a blocking call in an operation's answer throws it), and a guard's finalizer —
   * a resource's release — runs for it as Java's `finally` does; `NonFatal` counts it fatal, which let a cancelled
   * fiber leave its guards unrun (bracket-cancel-releases, okay's TestAsync). It reaches the top as itself */
  def raisable(e: Throwable): Boolean = NonFatal(e) || e.isInstanceOf[InterruptedException]

  /** what user code threw, a `Raise` — a fatal error thrown on: the handler at each call one call, kept small */
  private def raised[R <: Row, I, O, A](e: Throwable): Cont[R, I, O, A] = if raisable(e) then Raise(e) else throw e

  /** AN ERROR RAISED: the frames of each level dropped, the levels unwound innermost first — a state's register gone
   * with its level — to the top, its answer to it */
  @tailrec private def unwind[Z](e: Throwable, m: Meta[?, ?, ?, ?, Z]): Next[Z] = m match
    case u: Meta.Under[e2, t, v, b, n, v2, j2, o2, z] => e match
      case to: AbortTo => u.pending match
        case p if p eq to.pending => Eff(u.none.flip.liftCo[[x] =>> Cont[t, x, Nothing, b]](p), u.k, u.m)
        case _ => unwind(e, u.m)
      case _ => unwind(e, u.m)
    case u: Meta.UnderGuard[t, v, b, j, v2, j2, o, z] if u.g.masked && cancelAsked(u.m) =>
      // a mask left by an error, a cancel asked under it: the region's end cancels still — unwound on as a cancel
      unwind(Cancelled, u.m)
    case u: Meta.UnderGuard[t, v, b, j, v2, j2, o, z] =>
      // a recovering guard: its program in the body's place, a program of effects as the body is (`none`, by the types)
      val recovered: Cont[t, j, Nothing, b] | Null = u.none match
        case null => null
        case none => u.g match
          case r: Recovering[t, v, b] => e match
            case _: Exit => null
            case _ => (try r.recover(e) catch case e2: Throwable => raised[t, Nothing, Nothing, b](e2)) match
              case null => null
              case p => none.flip.liftCo[[x] =>> Cont[t, x, Nothing, b]](p)
          case _ => null
      recovered match
        case null => u.g.finalizer match
          case null => unwind(e, u.m)
          // the finalizer where the guard was, the frames outside it dropped; then the unwinding on
          case fin => Eff[t, j2, o, v2, v2, j2, Z](Bind(fin, (_: Unit) => Raise[t, j2, Nothing, v2](e)),
            Frame.end, u.m)
        case p => Eff[t, j, o, b, v2, j2, Z](p, u.k, u.m)
    case t: Meta.Top[r, v, z] => Done(t.fail(e))
    case t: Meta.Through[e2, t2, v, b, j, v2, j2, o, z] => unwind(e, t.m)
    case u: Meta.UnderState[e2, t, s, v, b, j, v2, j2, o, z] => unwind(e, u.m)
    case u: Meta.UnderView[e2, r, t, v, j, v2, j2, o, z] => unwind(e, u.m)
    case l: Meta.Lifted[r, a, s, v, j, o, z] => unwind(e, l.m)
    case d: Meta.Dollar[t, a, j, i2, o2, b, v2, j2, z] => unwind(e, d.m)
    case r: Meta.Reset[r2, s, o, n, v2, j2, o2, z] => unwind(e, r.m)
    case u: Meta.UnderObserve[t, v, b, n, v2, j2, o2, z] => unwind(e, u.m)

  // LOOKING UP A HANDLER

  /** the handler of `E` at `in`, found by the path, nothing taken: answering in place, its level with a state, or an
   * aborting one's — else `null`, one taking `k`, to be made. `$`s passed as any effect is */
  type Level[E[_], Z] = Answering[E, ?, ?, ?] | Meta.UnderState[E, ?, ?, ?, ?, ?, ?, ?, ?, ?] |
    Meta.Under[E, ?, ?, ?, ?, ?, ?, ?, ? <: Z] | Null

  @tailrec private def level[E[_], R <: Row, Z](in: In[E, R], m: Meta[R, ?, ?, ?, Z]): Level[E, Z] = m match
    case u: Meta.Under[e, t, v, b, n, v2, j2, o2, z] => in match
      case In.Here() => u.h match
        case _: Aborting[e4, t4, v4, b4] => u
        case _ => null
      case In.There(in) => level(in, u.m)
      // a place by key: the handler asked whether the key is its own — then as the head; else on, the same place
      case at: In.At[e3, r3] => u.h.at(at) match
        case null => level(at.again[t], u.m)
        case s: In.Same[e, E] => s match
          case In.Same.Refl() => u.h match
            case _: Aborting[e4, t4, v4, b4] => u
            case _ => null
    case th: Meta.Through[e, t, v, b, j, v2, j2, o, z] => in match
      case In.Here() => th.h match
        case a: Answering[e4, t4, v4, b4] => a
        case _ => null
      case In.There(in) => level(in, th.m)
      case at: In.At[e3, r3] => th.h.at(at) match
        case null => level(at.again[t], th.m)
        case s: In.Same[e, E] => s match
          case In.Same.Refl() => th.h match
            case a: Answering[e4, t4, v4, b4] => a
            case _ => null
    case u: Meta.UnderState[e, t, s, v, b, j, v2, j2, o, z] => in match
      case In.Here() => u
      case In.There(in) => level(in, u.m)
      case at: In.At[e3, r3] => u.h.at(at) match
        case null => level(at.again[t], u.m)
        case s: In.Same[e, E] => s match
          case In.Same.Refl() => u
    case u: Meta.UnderView[e, r, t, v, j, v2, j2, o, z] => level(Removed.relocate(in, u.rm), u.m)
    case d: Meta.Dollar[t, a, j, i2, o2, b, v2, j2, z] => in match
      case In.Here() => null
      case In.There(in) => level(in, d.m)
      case at: In.At[e3, r3] => level(at.again[t], d.m)
    case r: Meta.Reset[r2, s, o, n, v2, j2, o2, z] => in match
      case In.Here() => null
      case In.There(in) => level(in, r.m)
      case at: In.At[e3, r3] => level(at.again[r2], r.m)
    case _ => m.passed match
      case null => null
      case o => level[E, R, Z](in, o)

  /** an operation, its handler FOUND by its path: answered in place, its value to its rest `g` at once — no node made
   * for it; an abort, the levels between dropped; else out to its handler, `outE`. Inlined into the loop: its calls of
   * `eval` the loop's tail calls */
  private inline def found[E[_], Y, R <: Row, I, O, A, V, J, Z](op: E[Y], in: In[E, R], g: Y => Cont[R, I, O, A],
      k: Frame[R, A, I, V, J], m: Meta[R, V, J, O, Z]): Next[Z] = lookUp[E, R, Z](in, m) match
    case a: Answering[e4, t4, v4, b4] => eval(guarded(g(a.value[Y](op))), k, m)
    case null => outE(op, in, g, null, k, m)
    case u: Meta.UnderState[e2, t2, s2, v, b, j, v2, j2, o, z] =>
      eval(guarded { val value = u.h.value[Y](u.s, op); u.s = u.h.next(u.s, op); g(value) }, k, m)
    case u: Meta.Under[e2, t2, v, b, n, v2, j2, o2, z] => aborted(u, op, in, g, k, m)

  /** `found` as a step of its own, out of the loop: for a place made in the loop — an operation at the head seen
   * from outside its handler, or at the top — not worth the loop's room */
  private def foundOff[E[_], Y, R <: Row, I, O, A, V, J, Z](op: E[Y], in: In[E, R], g: Y => Cont[R, I, O, A],
      k: Frame[R, A, I, V, J], m: Meta[R, V, J, O, Z]): Next[Z] = lookUp[E, R, Z](in, m) match
    case a: Answering[e4, t4, v4, b4] => Eff(guarded(g(a.value[Y](op))), k, m)
    case null => outE(op, in, g, null, k, m)
    case u: Meta.UnderState[e2, t2, s2, v, b, j, v2, j2, o, z] =>
      Eff(guarded { val value = u.h.value[Y](u.s, op); u.s = u.h.next(u.s, op); g(value) }, k, m)
    case u: Meta.Under[e2, t2, v, b, n, v2, j2, o2, z] => aborted(u, op, in, g, k, m)

  /** an operation found at a level taking the rest — an abort's (`level` gives such a level for an `Aborting` only):
   * its program there, the levels between dropped — or, with a guard among them, unwound to it, the program
   * pending. Any other handler there: out to it the usual way */
  private def aborted[E[_], T <: Row, V, B, N, V2, J2, O2, Z, Y, R <: Row, I, O, A, V1, J1, Z1 <: Z](
      u: Meta.Under[E, T, V, B, N, V2, J2, O2, Z1], op: E[Y], in: In[E, R], g: Y => Cont[R, I, O, A],
      k: Frame[R, A, I, V1, J1], m: Meta[R, V1, J1, O, Z]): Next[Z] = u.h match
    case ab: Aborting[e4, t4, v4, b4] =>
      if guardsTo(m, u) then
        val p = guarded(ab.abort[Y](op))
        u.pending = p
        unwind(AbortTo(p), m)
      else Eff(u.none.flip.liftCo[[x] =>> Cont[T, x, Nothing, B]](guarded(ab.abort[Y](op))), u.k, u.m)
    case _ => outE(op, in, g, null, k, m)

  /** an operation answered by a state in place: its value to the rest `g`, the register moved on — none moved if
   * user code throws */
  private def stated[E[_], T <: Row, S, V, B, J, V2, J2, O, Z, Y, R <: Row, I, O1, A](
      u: Meta.UnderState[E, T, S, V, B, J, V2, J2, O, Z], op: E[Y], g: Y => Cont[R, I, O1, A]): Cont[R, I, O1, A] =
    try
      val value = u.h.value[Y](u.s, op)
      u.s = u.h.next(u.s, op)
      g(value)
    catch case e: Throwable => raised(e)

  /** A LOOK-UP KEPT on the path: the handler answering in place, or the state's level, found from `at` — the same
   * levels, the same answer while `at` is where the operation is */
  final class Found[E[_]](val at: AnyRef, val level: Answering[E, ?, ?, ?] | Meta.UnderState[E, ?, ?, ?, ?, ?, ?, ?, ?, ?])
      extends In.Cache[E]

  /** `level`, the path's memory first: from the same level, its handler at once */
  private def lookUp[E[_], R <: Row, Z](in: In[E, R], m: Meta[R, ?, ?, ?, Z]): Level[E, Z] = in.cache match
    case f: Found[e] if f.at eq m => f.level match
      case a: Answering[e4, t4, v4, b4] => a
      case u: Meta.UnderState[e2, t2, s2, v2, b, j, v3, j3, o, z] => u
    case _ =>
      val l = level[E, R, Z](in, m)
      l match
        case a: Answering[e4, t4, v4, b4] => in.cache = Found[E](m, a)
        case u: Meta.UnderState[e2, t2, s2, v2, b, j, v3, j3, o, z] => in.cache = Found[E](m, u)
        case _ => ()
      l

  /** a guard among the levels from `m` to `to`: an abort to `to` unwinding them, not a jump */
  @tailrec private def guardsTo(m: Meta[?, ?, ?, ?, ?] | Null, to: AnyRef): Boolean = m match
    case null => false
    case _ if m eq to => false
    case _: Meta.UnderGuard[t, v, b, j, v2, j2, o, z] => true
    case n: Meta[?, ?, ?, ?, ?] => guardsTo(n.outer, to)

  /** a clause of a handler taking the rest, at its level: `Deep`'s with `k`, `Aborting`'s without — its program at the
   * frames outside the level, at no answer */
  private def clausing[E[_], T <: Row, V, B, N, V2, J2, O2, Z, Y](u: Meta.Under[E, T, V, B, N, V2, J2, O2, Z], op: E[Y],
      k: Rest[Y, T, N, N, B]): Cont[T, N, Nothing, B] =
    val kk = u.none.liftCo[[x] =>> Rest[Y, T, x, x, B]](k)
    val p: B ! T =
      try u.h match
        case d: Deep[e, t, a, b] => d[Y](op, kk)
        case ab: Aborting[e, t, a, b] => ab.abort[Y](op)
      catch case e: Throwable => raised(e)
    u.none.flip.liftCo[[x] =>> Cont[T, x, Nothing, B]](p)

  /** a clause of a handler leaving the rest, at its level, the answers through: `Translating`'s program then the
   * rest, `Linear`'s with it, `Answering`'s the rest with its value */
  /** an observer's clause, at its level: the operation seen, with `k` — its program at the frames outside the level */
  private def observing[E[_], T <: Row, V, B, N, V2, J2, O2, Z, Y](u: Meta.UnderObserve[T, V, B, N, V2, J2, O2, Z],
      op: E[Y], in: In[E, T], k: Rest[Y, T, N, N, B]): Cont[T, N, Nothing, B] =
    val kk = u.none.liftCo[[x] =>> Rest[Y, T, x, x, B]](k)
    val p: B ! T =
      try u.o[E, Y](Seen(op, in), kk)
      catch case e: Throwable => raised(e)
    u.none.flip.liftCo[[x] =>> Cont[T, x, Nothing, B]](p)

  private def passing[E[_], T <: Row, A, B, J, O, Y](h: Passing[E, T, A, B], op: E[Y], k: Rest[Y, T, J, O, B])
      : Cont[T, J, O, B] =
    try h match
      case a: Answering[e, t, a1, b] => k(a.value[Y](op))
      case tr: Translating[e, t, a1, b] => Bind(Lift[T, O, Y](tr[Y](op)), k)
      case ln: Linear[e, t, a1, b] => ln[J, O, Y](op, k)
    catch case e: Throwable => raised(e)

  // THE LOOP

  @tailrec def eval[R <: Row, I, O, A, V, J, Z](c: Cont[R, I, O, A], k: Frame[R, A, I, V, J], m: Meta[R, V, J, O, Z])
      : Next[Z] = c match
    case Return(a) => k match
      case Frame.Bind(f, rest) => eval(guarded(f(a)), rest, m)
      case mf: Frame.Map[r, x, i, a1, v, j] => mf.rest match
        // two maps: both functions at once — no value between them made into a program
        case mf2: Frame.Map[r2, x2, i2, a2, v2, j2] => eval(guarded(Return(mf2.f(mf.f(a)))), mf2.rest, m)
        case _ => eval(guarded(Return(mf.f(a))), mf.rest, m)
      case Frame.End() => m match
        case t: Meta.Top[r, v, z] => Done(t.k(a))
        case th: Meta.Through[e, t, v, b, j, v2, j2, o, z] => eval(Lift[t, j, b](guarded(th.h.ret(a))), th.k, th.m)
        case u: Meta.UnderState[e, t, s, v, b, j, v2, j2, o, z] => eval(Lift[t, j, b](guarded(u.h.ret(u.s, a))), u.k, u.m)
        case u: Meta.Under[e, t, v, b, n, v2, j2, o2, z] =>
          eval[t, n, o2, b, v2, j2, Z](u.none.flip.liftCo[[x] =>> Cont[t, x, Nothing, b]](guarded(u.h.ret(a))), u.k, u.m)
        case u: Meta.UnderView[e, r, t, v, j, v2, j2, o, z] => eval(Return[e +: t, j, v](a), u.k, u.m)
        case _ => bottom(a, m, summon[J <:< O])
    case b: Bind[r, i, t, o, x, a] => b.m match
      case Return(a) => eval(guarded(b.f(a)), k, m) // a value bound: on at once, no frame
      // a handler's level resumed, bound: entered here at once, the bind its outer frame; its rest a value, nothing
      // after it — the handler's `ret`, no level entered for it
      case rs: Resume[y, t2, i2, o2, b2] => rs.c match
        case cn: Rest.Under[y2, e, t3, x2, i3, a1, b3, n] => cn.k match
          case Frame.End() =>
            val body: Cont[e +: t3, i3, n, x2] =
              try cn.inner(rs.y) catch case err: Throwable => raised[e +: t3, i3, n, x2](err)
            body match
              case Return(v) =>
                val ret: b3 ! t3 = guarded(cn.h.ret(v))
                ret match
                  case Return(bv) => eval(guarded(b.f(bv)), k, m)
                  case r => eval(cn.none.flip.liftCo[[x] =>> Cont[t3, x, Nothing, b3]](r), Frame.Bind(b.f, k), m)
              case _ => eval[e +: t3, i3, n, x2, a1, n, Z](body, cn.k, Meta.Under(cn.h, Frame.Bind(b.f, k), m, cn.none, cn.pending))
          case _ => eval[e +: t3, i3, n, x2, a1, n, Z](guarded(cn.inner(rs.y)), cn.k, Meta.Under(cn.h, Frame.Bind(b.f, k), m, cn.none, cn.pending))
        case _ => eval(b.m, Frame.Bind(b.f, k), m)
      case _ => eval(b.m, Frame.Bind(b.f, k), m)
    case mp: Map[r, i, o, x, a] => mp.m match
      case Return(a) => eval(guarded(Return(mp.f(a))), k, m)
      case _ => eval(mp.m, Frame.Map(mp.f, k), m)
    // an operation of the row's head: its handler the nearest level, if that is at the head — no path walked; with no
    // rest, answered in place, its value straight into the frame on top
    case p: PerformHere[e, t, s, x] => m match
      case th: Meta.Through[e2, t2, v, b, j, v2, j2, o, z] => th.h match
        case a: Answering[e4, t4, v4, b4] => k match
          case Frame.Bind(f, rest) => eval(guarded(f(a.value[x](p.op))), rest, m)
          case Frame.Map(f, rest) => eval(guarded(Return(f(a.value[x](p.op)))), rest, m)
          case Frame.End() => eval(guarded(Return[R, s, x](a.value[x](p.op))), k, m)
        case h => eval(passing(h, p.op, Rest.Through(h, k, (y: x) => Return[R, s, x](y), null)), th.k, th.m)
      case u: Meta.UnderState[e2, t2, s2, v, b, j, v2, j2, o, z] => k match
        case Frame.Bind(f, rest) => eval(guarded { val value = u.h.value[x](u.s, p.op); u.s = u.h.next(u.s, p.op); f(value) }, rest, m)
        case Frame.Map(f, rest) => eval(guarded { val value = u.h.value[x](u.s, p.op); u.s = u.h.next(u.s, p.op); Return(f(value)) }, rest, m)
        case Frame.End() => eval(guarded { val value = u.h.value[x](u.s, p.op); u.s = u.h.next(u.s, p.op); Return[R, s, x](value) }, k, m)
      case u: Meta.Under[e2, t2, v, b, n, v2, j2, o2, z] =>
        eval(clausing(u, p.op, Rest.Under(u.h, k, (y: x) => Return[R, s, x](y), null, u.none, u.pending)), u.k, u.m)
      // seen from outside a handler of another effect, or the top: its place made, found the usual way
      case _ => foundOff(p.op, In.Here[e, t](), (y: x) => Return[R, s, x](y), k, m)
    case ih: InjectHere[e, t, y, i, o, a] => m match
      case th: Meta.Through[e2, t2, v, b, j, v2, j2, o2, z] => th.h match
        case a: Answering[e4, t4, v4, b4] => eval(guarded(ih.g(a.value[y](ih.op))), k, m)
        case h => eval(passing(h, ih.op, Rest.Through(h, k, ih.g, null)), th.k, th.m)
      case u: Meta.UnderState[e2, t2, s2, v, b, j, v2, j2, o2, z] =>
        eval(guarded { val value = u.h.value[y](u.s, ih.op); u.s = u.h.next(u.s, ih.op); ih.g(value) }, k, m)
      case u: Meta.Under[e2, t2, v, b, n, v2, j2, o2, z] =>
        eval(clausing(u, ih.op, Rest.Under(u.h, k, ih.g, null, u.none, u.pending)), u.k, u.m)
      case _ => foundOff(ih.op, In.Here[e, t](), ih.g, k, m)
    // an operation with its place, not the head: its handler found by its path
    case p: Perform[r, e, s, x] => found(p.op, p.in, (y: x) => Return[R, s, x](y), k, m)
    case op: Inject[r, e, y, i, o, a] => found(op.op, op.in, op.g, k, m)
    // a `k` resumed: its outermost level put back, its frames those inside it; the rest, a `Resume` of the inner levels
    // or `inner(y)` itself, under it
    case rs: Resume[y, t, i, o, b] => rs.c match
      case cn: Rest.Under[y2, e, t2, x, i2, a, b2, n] =>
        eval[e +: t2, i2, n, x, a, n, Z](guarded(cn.inner(rs.y)), cn.k, Meta.Under(cn.h, k, m, cn.none, cn.pending))
      case cn: Rest.Through[y2, e, t2, x, i2, a, j, o2, b2] =>
        eval[e +: t2, i2, O, x, a, j, Z](guarded(cn.inner(rs.y)), cn.k, Meta.Through(cn.h, k, m))
      case cn: Rest.UnderState[y2, e, t2, s2, x, i2, a, j, o2, b2] =>
        eval[e +: t2, i2, O, x, a, j, Z](guarded(cn.inner(rs.y)), cn.k, Meta.UnderState(cn.h, cn.s, k, m))
      case _ => resumed(rs, k, m)
    // a handler entered: its level, the frames outside it those here; its body on its own frames
    case h: Handle[e, t, x, i, a, j, o, b] => eval(h.body, h.k, Meta.Through(h.h, k, m))
    case h: HandleState[e, t, st, x, i, a, j, o, b] => eval(h.body, h.k, Meta.UnderState(h.h, h.s, k, m))
    case h: HandleK[e, t, x, i, a, b] => eval(h.body, h.k, Meta.Under(h.h, k, m, summon[I =:= Nothing], null))
    case v: View[e, r, t, x, i, a, j, o] => eval(v.body, v.k, Meta.UnderView(v.rm, k, m))
    case Defer(p) => eval(guarded(p()), k, m)
    case _ => rare(c, k, m)

  /** the rest of the nodes, out of the loop — control's, a view's, a guard's, a failure's: each a step of its own */
  private def rare[R <: Row, I, O, A, V, J, Z](c: Cont[R, I, O, A], k: Frame[R, A, I, V, J], m: Meta[R, V, J, O, Z]): Next[Z] =
    c match
      case v: View[e, r, t, x, i, a, j, o] => Eff(v.body, v.k, Meta.UnderView(v.rm, k, m))
      // a program of effects at an answer: its level, no answer inside
      case l: Lift[r, s, a] => Eff(l.p, Frame.end, Meta.Lifted[r, a, s, V, J, O, Z](k, m, summon[s <:< O]))
      // a `$`: its level, its body on its own frames, the `ret` at its bottom; a `reset`'s the same, the identity
      case d: Dollar[t, i, i2, o2, b, a] => Eff(d.body, Frame.end, Meta.Dollar(d.ret, k, m))
      case l: Lower[r, s, o] => Eff(l.c, Frame.end, Meta.Reset(k, m, summon[I =:= Nothing]))
      // a shift0: out to its `$` as an operation goes out to its handler, the levels passed into its `k`
      case s: Shift0[r, t, i1, o1, x1, o, a] =>
        shifted(s.at, s.f, (y: a) => Return[r, Cont[t, i1, o1, x1], a](y), null, k, m)
      case g: Guarded[t, x, i, a, j, o, b] => Eff(g.body, g.k, Meta.UnderGuard(g.g, k, m, g.none))
      case o: Observed[t, x, i, a, b] => Eff(o.body, o.k, Meta.UnderObserve(o.o, k, m, summon[I =:= Nothing]))
      case fl: Fail[t, i, o, b] => failIn(fl, k, m)
      case Raise(e) => unwind(e, m)
      case _ => Eff(c, k, m)

  /** a value at the bottom of a level not a handler's: a view's, a lift's, a `$`'s, a guard's */
  private def bottom[R <: Row, V, J, O, Z](a: V, m: Meta[R, V, J, O, Z], up: J <:< O): Next[Z] = m match
    case l: Meta.Lifted[r, a2, s, v, j, o, z] => Eff[r, s, o, a2, v, j, Z](l.up.liftCo[[x] =>> Cont[r, s, x, a2]](Return(a)), l.k, l.m)
    case d: Meta.Dollar[t, a2, j, i2, o2, b, v2, j2, z] =>
      try
        val p: Cont[t, i2, o2, b] = up(d.ret(a))
        Eff[t, i2, o2, b, v2, j2, Z](p, d.k, d.m)
      catch case e: Throwable if raisable(e) => unwind(e, m)
    case r: Meta.Reset[r2, s, o, n, v2, j2, o2, z] =>
      Eff[r2, n, o2, o, v2, j2, Z](r.none.flip.liftCo[[x] =>> Cont[r2, x, Nothing, o]](Return[r2, Nothing, o](up(a))), r.k, r.m)
    case u: Meta.UnderGuard[t, v, b, j, v2, j2, o, z] =>
      if u.g.masked then maskLeft(u, a, up)
      else Eff[t, j, o, b, v2, j2, Z](left(u, a, up), u.k, u.m)
    case u: Meta.UnderView[e, r, t, v, j, v2, j2, o, z] =>
      Eff[e +: t, j, o, v, v2, j2, Z](up.liftCo[[x] =>> Cont[e +: t, j, x, v]](Return(a)), u.k, u.m)
    case u: Meta.UnderObserve[t, v, b, n, v2, j2, o2, z] =>
      Eff[t, n, o2, b, v2, j2, Z](u.none.flip.liftCo[[x] =>> Cont[t, x, Nothing, b]](guarded(u.o.ret(a))), u.k, u.m)
    case t: Meta.Top[r, v, z] => Done(t.k(a))
    case th: Meta.Through[e, t, v, b, j, v2, j2, o, z] =>
      Eff[t, j, o, b, v2, j2, Z](up.liftCo[[x] =>> Cont[t, j, x, b]](Lift[t, j, b](guarded(th.h.ret(a)))), th.k, th.m)
    case u: Meta.UnderState[e, t, s, v, b, j, v2, j2, o, z] =>
      Eff[t, j, o, b, v2, j2, Z](up.liftCo[[x] =>> Cont[t, j, x, b]](Lift[t, j, b](guarded(u.h.ret(u.s, a)))), u.k, u.m)
    case u: Meta.Under[e, t, v, b, n, v2, j2, o2, z] =>
      Eff[t, n, o2, b, v2, j2, Z](u.none.flip.liftCo[[x] =>> Cont[t, x, Nothing, b]](guarded(u.h.ret(a))), u.k, u.m)

  /** a guard's level put back by a resumption: what the guard asked to be run at each (`Guard.passing`) */
  private def resuming(r: (() => Unit) | Null): Unit = r match
    case null => ()
    case f: (() => Unit) => f()

  /** a `k` resumed, its outermost level one not a handler's: put back, the rest under it */
  private def resumed[Y, T <: Row, I, O, B, V, J, O2 >: O, Z](rs: Resume[Y, T, I, O, B], k: Frame[T, B, I, V, J],
      m: Meta[T, V, J, O2, Z]): Next[Z] = rs.c match
    case cn: Rest.UnderView[y2, e, r, t2, x, i2, a, j, o2] =>
      Eff[r, i2, O2, x, a, j, Z](guarded(cn.inner(rs.y)), cn.k, Meta.UnderView(cn.rm, k, m))
    case cn: Rest.UnderLifted[y2, r, x, i2, a, s] =>
      Eff[r, i2, Nothing, x, a, Nothing, Z](guarded(cn.inner(rs.y)), cn.k, Meta.Lifted[r, a, s, V, J, O2, Z](k, m, summon[s <:< O2]))
    case cn: Rest.UnderDollar[y2, t2, x, i2, a, j, i3, o3, b2] =>
      Eff[Control[t2] +: t2, i2, Cont[t2, i3, O2, b2], x, a, j, Z](guarded(cn.inner(rs.y)), cn.k, Meta.Dollar(cn.ret, k, m))
    case cn: Rest.UnderReset[y2, r, x, i2, s, o2, n] =>
      Eff[Control[r] +: r, i2, o2, x, s, s, Z](guarded(cn.inner(rs.y)), cn.k, Meta.Reset(k, m, cn.none))
    case cn: Rest.UnderGuard[y2, t2, x, i2, a, j, o2, b2] =>
      Eff[t2, i2, O2, x, a, j, Z](guarded { resuming(cn.resumed); cn.inner(rs.y) }, cn.k, Meta.UnderGuard(cn.g, k, m, cn.none))
    case cn: Rest.UnderObserve[y2, t2, x, i2, a, b2, n] =>
      Eff[t2, i2, n, x, a, n, Z](guarded(cn.inner(rs.y)), cn.k, Meta.UnderObserve(cn.o, k, m, cn.none))
    case cn: Rest.Under[y2, e, t2, x, i2, a, b2, n] =>
      Eff[e +: t2, i2, n, x, a, n, Z](guarded(cn.inner(rs.y)), cn.k, Meta.Under(cn.h, k, m, cn.none, cn.pending))
    case cn: Rest.Through[y2, e, t2, x, i2, a, j, o2, b2] =>
      Eff[e +: t2, i2, O2, x, a, j, Z](guarded(cn.inner(rs.y)), cn.k, Meta.Through(cn.h, k, m))
    case cn: Rest.UnderState[y2, e, t2, s2, x, i2, a, j, o2, b2] =>
      Eff[e +: t2, i2, O2, x, a, j, Z](guarded(cn.inner(rs.y)), cn.k, Meta.UnderState(cn.h, cn.s, k, m))

  /** a `k` failed: its outermost level put back, then the error raised inside the rest — its own levels, or here */
  private def failIn[T <: Row, I, O, B, V, J, O2 >: O, Z](fl: Fail[T, I, O, B], k: Frame[T, B, I, V, J],
      m: Meta[T, V, J, O2, Z]): Next[Z] = fl.c match
    case cn: Rest.Under[y, e, t2, x, i2, a, b2, n] =>
      Eff[e +: t2, i2, n, x, a, n, Z](failed(cn.fails, fl.e), cn.k, Meta.Under(cn.h, k, m, cn.none, cn.pending))
    case cn: Rest.Through[y, e, t2, x, i2, a, j, o2, b2] =>
      Eff[e +: t2, i2, O2, x, a, j, Z](failed(cn.fails, fl.e), cn.k, Meta.Through(cn.h, k, m))
    case cn: Rest.UnderState[y, e, t2, s, x, i2, a, j, o2, b2] =>
      Eff[e +: t2, i2, O2, x, a, j, Z](failed(cn.fails, fl.e), cn.k, Meta.UnderState(cn.h, cn.s, k, m))
    case cn: Rest.UnderView[y, e, r, t2, x, i2, a, j, o2] =>
      Eff[r, i2, O2, x, a, j, Z](failed(cn.fails, fl.e), cn.k, Meta.UnderView(cn.rm, k, m))
    case cn: Rest.UnderGuard[y, t2, x, i2, a, j, o2, b2] =>
      Eff[t2, i2, O2, x, a, j, Z](guarded { resuming(cn.resumed); failed(cn.fails, fl.e) }, cn.k,
        Meta.UnderGuard(cn.g, k, m, cn.none))
    case cn: Rest.UnderObserve[y, t2, x, i2, a, b2, n] =>
      Eff[t2, i2, n, x, a, n, Z](failed(cn.fails, fl.e), cn.k, Meta.UnderObserve(cn.o, k, m, cn.none))
    case cn: Rest.UnderLifted[y, r, x, i2, a, s] =>
      Eff[r, i2, Nothing, x, a, Nothing, Z](failed(cn.fails, fl.e), cn.k, Meta.Lifted[r, a, s, V, J, O2, Z](k, m, summon[s <:< O2]))
    case cn: Rest.UnderDollar[y, t2, x, i2, a, j, i3, o3, b2] =>
      Eff[Control[t2] +: t2, i2, Cont[t2, i3, O2, b2], x, a, j, Z](failed(cn.fails, fl.e), cn.k, Meta.Dollar(cn.ret, k, m))
    case cn: Rest.UnderReset[y, r, x, i2, s, o2, n] =>
      Eff[Control[r] +: r, i2, o2, x, s, s, Z](failed(cn.fails, fl.e), cn.k, Meta.Reset(k, m, cn.none))

  // OUT TO A HANDLER TAKING `k`, OR TO A `$`

  /** an operation ON ITS WAY OUT to a handler taking `k`, past levels: each level passed put around its rest `g`, a
   * `Rest`; at the handler's level, its clause with it — the state there; at the top, given back. Each step a tail
   * call */
  @tailrec private def outE[E[_], Y, R <: Row, I, O, A, V, J, Z](op: E[Y], in: In[E, R], g: Y => Cont[R, I, O, A],
      gf: Fails[Cont[R, I, O, A]] | Null, k: Frame[R, A, I, V, J], m: Meta[R, V, J, O, Z]): Next[Z] = m match
    case u: Meta.Under[e, t, v, b, n, v2, j2, o2, z] => in match
      case In.Here() => Eff(clausing(u, op, Rest.Under(u.h, k, g, gf, u.none, u.pending)), u.k, u.m)
      case In.There(in) =>
        val c = u.none.liftCo[[x] =>> Rest[Y, t, n, x, b]](Rest.Under(u.h, k, g, gf, u.none, u.pending))
        outE[E, Y, t, n, o2, b, v2, j2, Z](op, in, c, c, u.k, u.m)
      // a place by key: the handler's own — as the head; else past it, the same place
      case at: In.At[e3, r3] => u.h.at(at) match
        case null =>
          val c = u.none.liftCo[[x] =>> Rest[Y, t, n, x, b]](Rest.Under(u.h, k, g, gf, u.none, u.pending))
          outE[E, Y, t, n, o2, b, v2, j2, Z](op, at.again[t], c, c, u.k, u.m)
        case s: In.Same[e, E] => s match
          case In.Same.Refl() => Eff(clausing(u, op, Rest.Under(u.h, k, g, gf, u.none, u.pending)), u.k, u.m)
    case th: Meta.Through[e, t, v, b, j, v2, j2, o, z] => in match
      case In.Here() => Eff(passing(th.h, op, Rest.Through(th.h, k, g, gf)), th.k, th.m)
      case In.There(in) =>
        val c = Rest.Through(th.h, k, g, gf)
        outE(op, in, c, c, th.k, th.m)
      case at: In.At[e3, r3] => th.h.at(at) match
        case null =>
          val c = Rest.Through(th.h, k, g, gf)
          outE(op, at.again[t], c, c, th.k, th.m)
        case s: In.Same[e, E] => s match
          case In.Same.Refl() => Eff(passing(th.h, op, Rest.Through(th.h, k, g, gf)), th.k, th.m)
    case u: Meta.UnderState[e, t, s, v, b, j, v2, j2, o, z] => in match
      case In.Here() => Eff(stated(u, op, g), k, m)
      case In.There(in) =>
        val c = Rest.UnderState(u.h, u.s, k, g, gf)
        outE(op, in, c, c, u.k, u.m)
      case at: In.At[e3, r3] => u.h.at(at) match
        case null =>
          val c = Rest.UnderState(u.h, u.s, k, g, gf)
          outE(op, at.again[t], c, c, u.k, u.m)
        case sm: In.Same[e, E] => sm match
          case In.Same.Refl() => Eff(stated(u, op, g), k, m)
    case u: Meta.UnderView[e, r, t, v, j, v2, j2, o, z] =>
      val c = Rest.UnderView(u.rm, k, g, gf)
      outE(op, Removed.relocate(in, u.rm), c, c, u.k, u.m)
    // a guard: told of the operation — what goes out in its place, what is run at each resumption; what it throws
    // raised where the operation is, inside it
    case u: Meta.UnderGuard[t, v, b, j, v2, j2, o, z] =>
      val out: Guard.Out[E, Y] | Throwable = try u.g.passing(op) catch case e: Throwable => if raisable(e) then e else throw e
      out match
        case null =>
          val c = Rest.UnderGuard(u.g, k, g, gf, u.none, null)
          outE(op, in, c, c, u.k, u.m)
        case e: Throwable => Eff(failed(gf, e), k, m)
        case o: Guard.Out[E, Y] @unchecked =>
          val c = Rest.UnderGuard(u.g, k, g, gf, u.none, o.resumed)
          outE(o.op, in, c, c, u.k, u.m)
    case l: Meta.Lifted[r, a, s, v, j, o, z] =>
      val c = l.up.liftCo[[x] =>> Rest[Y, r, s, x, a]](Rest.UnderLifted[Y, r, A, I, a, s](k, g, gf))
      outE[E, Y, r, s, o, a, v, j, Z](op, in, c, c, l.k, l.m)
    // an observer: the operation stops here, its clause given it seen and the rest so far
    case u: Meta.UnderObserve[t, v, b, n, v2, j2, o2, z] =>
      Eff(observing(u, op, in, Rest.UnderObserve(u.o, k, g, gf, u.none)), u.k, u.m)
    case d: Meta.Dollar[t, a, j, i2, o2, b, v2, j2, z] => in match
      case In.Here() => ControlOp.absurd(op)
      case In.There(in) =>
        val c = Rest.UnderDollar[Y, t, A, I, a, j, i2, o2, b](d.ret, k, g, gf)
        outE(op, in, c, c, d.k, d.m)
      case at: In.At[e3, r3] =>
        val c = Rest.UnderDollar[Y, t, A, I, a, j, i2, o2, b](d.ret, k, g, gf)
        outE(op, at.again[t], c, c, d.k, d.m)
    case r: Meta.Reset[r2, s, o, n, v2, j2, o2, z] => in match
      case In.Here() => ControlOp.absurd(op)
      case In.There(in) =>
        val c = r.none.liftCo[[x] =>> Rest[Y, r2, n, x, o]](Rest.UnderReset[Y, r2, A, I, s, o, n](k, g, gf, r.none))
        outE[E, Y, r2, n, o2, o, v2, j2, Z](op, in, c, c, r.k, r.m)
      case at: In.At[e3, r3] =>
        val c = r.none.liftCo[[x] =>> Rest[Y, r2, n, x, o]](Rest.UnderReset[Y, r2, A, I, s, o, n](k, g, gf, r.none))
        outE[E, Y, r2, n, o2, o, v2, j2, Z](op, at.again[r2], c, c, r.k, r.m)
    case t: Meta.Top[r, v, z] => t.out match
      // none comes to the top of a row with none — but a place by key, no handler of it in force: raised
      case _: Out.None[z2] => in match
        case at: In.At[e3, r3] => unwind(IllegalStateException(s"an operation at a place by key with no handler of it in force: ${at.ref}"), m)
        case null => In.absurd(in)
      case p: Out.Paused[r2, a2] =>
        Done(Head.Paused[r2, a2, E, Y, I, O, A, V, J](op, in.asInstanceOf[In[E, r2]], g, gf, k,
          t.asInstanceOf[Meta.Top[R, V, Head[r2, a2]]]).asInstanceOf[z])
      case Out.Given(give) =>
        lazy val back: z = give(op, in, (y: Y) => drive[z](eval[R, I, O, A, V, J, z](guarded(g(y)), k, t)),
          (e: Throwable) => drive[z](eval[R, I, O, A, V, J, z](failed(gf, e), k, t)),
          // a cancel: unwound from inside k's levels; under a mask, kept for its exit — the step given back
          () => if masked(gf) then { t.cancelAsked = true; back }
                else drive[z](eval[R, I, O, A, V, J, z](failed(gf, Cancelled), k, t)))
        Done(back)

  /** a shift0 ON ITS WAY OUT to its `$` by its path, past the handlers before it — each put around its rest, a `Rest`,
   * the state there; at the `$`, `f` given the rest with the `$` around it, its answer run outside the `$`. Past a
   * handler taking the rest, a guard, a lift, or at the top, the shift0's answer is `Nothing`: its body cannot
   * return, and its `k` cannot be resumed — there is no level to resume it under — so it raises */
  @tailrec private def shifted[T <: Row, I1, O1, X1, Y, R <: Row, X, I, O, V, J, Z](at: Nearest[T, R],
      f: (Y => Cont[T, I1, O1, X1]) => O, g: Y => Cont[R, I, Cont[T, I1, O1, X1], X],
      gf: Fails[Cont[R, I, Cont[T, I1, O1, X1], X]] | Null, k: Frame[R, X, I, V, J], m: Meta[R, V, J, O, Z]): Next[Z] =
    m match
      case th: Meta.Through[e, t, v, b, j, v2, j2, o, z] => at match
        case Nearest.There(at1) =>
          val c = Rest.Through[Y, e, t, X, I, v, j, Cont[T, I1, O1, X1], b](th.h, k, g, gf)
          shifted(at1, f, c, c, th.k, th.m)
        case _: Nearest.Here[t3] => Nearest.absurd(at)
      case u: Meta.UnderState[e, t, s, v, b, j, v2, j2, o, z] => at match
        case Nearest.There(at1) =>
          val c = Rest.UnderState[Y, e, t, s, X, I, v, j, Cont[T, I1, O1, X1], b](u.h, u.s, k, g, gf)
          shifted(at1, f, c, c, u.k, u.m)
        case _: Nearest.Here[t3] => Nearest.absurd(at)
      case u: Meta.UnderView[e, r, t, v, j, v2, j2, o, z] =>
        val c = Rest.UnderView(u.rm, k, g, gf)
        shifted(Nearest.relocate(at, u.rm), f, c, c, u.k, u.m)
      case d: Meta.Dollar[t, a, j, i2, o2, b, v2, j2, z] => at match
        case _: Nearest.Here[t3] =>
          val kk = Rest.UnderDollar[Y, t, X, I, a, j, I1, O1, X1](d.ret, k, g, gf)
          try Eff[t, i2, o2, b, v2, j2, Z](f(kk), d.k, d.m) catch case e: Throwable if raisable(e) => unwind(e, m)
        case Nearest.There(at1) => Nearest.absurd(at)
      case r: Meta.Reset[r2, s, o, n, v2, j2, o2, z] => at match
        case _: Nearest.Here[t3] =>
          val kk = Rest.UnderDollar[Y, r2, X, I, s, s, I1, O1, X1]((a: s) => a, k, g, gf)
          try Eff[r2, n, o2, o, v2, j2, Z](r.none.flip.liftCo[[x] =>> Cont[r2, x, Nothing, o]](Return[r2, Nothing, o](f(kk))), r.k, r.m)
          catch case e: Throwable if raisable(e) => unwind(e, m)
        case Nearest.There(at1) => Nearest.absurd(at)
      case u: Meta.Under[e, t, v, b, n, v2, j2, o2, z] => never(u.none.liftCo[[x] =>> (Y => Cont[T, I1, O1, X1]) => x](f), m)
      case u: Meta.UnderGuard[t, v, b, j, v2, j2, o, z] =>
        val c = Rest.UnderGuard[Y, t, X, I, v, j, Cont[T, I1, O1, X1], b](u.g, k, g, gf, u.none, null)
        shifted(at, f, c, c, u.k, u.m)
      case l: Meta.Lifted[r, a, s, v, j, o, z] => never(f, m)
      case u: Meta.UnderObserve[t, v, b, n, v2, j2, o2, z] => never(u.none.liftCo[[x] =>> (Y => Cont[T, I1, O1, X1]) => x](f), m)
      case t: Meta.Top[r, v, z] => never(f, m)

  /** a shift0 whose body cannot return: `f` given a `k` that raises */
  private def never[T <: Row, I1, O1, X1, Y, Z](f: (Y => Cont[T, I1, O1, X1]) => Nothing, m: Meta[?, ?, ?, ?, Z]): Next[Z] =
    try f((y: Y) => Raise[T, I1, O1, X1](IllegalStateException(
      "a shift0 with no answer, under a handler taking the rest or a lift: its k resumed")))
    catch case e: Throwable if raisable(e) => unwind(e, m)
