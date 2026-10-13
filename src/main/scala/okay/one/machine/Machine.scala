package okay.one.machine

import okay.one.*
import okay.one.Cont0.*
import okay.one.handler.*
import okay.one.row.*
import okay.one.step.Step
import scala.annotation.tailrec
import scala.util.control.NonFatal

/** THE MACHINE: a CEK machine over the programs of `Cont0` — control `c`, the frames `k` of its level, the levels `m`
 * outside: ONE `Meta`, a handler each, or a `$` (`Reset`, `Dollar`), or a program of effects in control (`Lifted`).
 * Two loops over it, one for each kind of code — effects' frames carry values to a handler's `ret`, control's carry
 * answers (Asai–Kameyama's typing) — each handing the other the state when the code changes kind: a `Next`.
 *
 * An operation goes to the handler of its place, past `$`s too: answered in place where it is found so, the levels
 * between untouched; else out past each level, each put around its rest — a `Cont`, `k` as data, holding the `$`s it
 * passed, put back on the `Meta` in force where it is resumed. A shift0 goes to the nearest `$`. A handler's place is
 * in its type: none past the top but those the top takes */
object Machine:
  /** `p`, no effects left: its value */
  def run[I0, O0, A](p: Cont0[Pure, I0, O0, A]): A =
    drive(Eff(p, Frame.End(), Meta.Top(identity[A], (e: Throwable) => throw e, Out.none[A])))

  /** `c`, a program of control with no effect, run, its rest `k`: its answer */
  def control[I, O, A](c: Cont0[Control[Pure], I, O, A], k: A => I): O =
    drive(Ctl(c, CFrame.Return(k), Meta.TopC[O, O](identity, (e: Throwable) => throw e)))

  /** `p` stepped: to its value, or to its first operation of `R` come to the top — given back with its rest */
  def stepped[R <: Row, I0, O0, A](p: Cont0[R, I0, O0, A]): Step[R, A] =
    drive(Eff(p, Frame.End(), Meta.Top((a: A) => Step.Done(a),
      (e: Throwable) => if e eq Cancelled then Step.Cancelled() else Step.Failed(e),
      Out.Given[R, Step[R, A]]([E[_], Y] => (op: E[Y], in: In[E, R], resume: Y => Step[R, A],
        fail: Throwable => Step[R, A], cancel: () => Step[R, A]) => Step.Yield(op, in, Step.Resume(resume, fail, cancel))))))

  // THE DATA

  /** THE FRAMES of a level of effects: a value of `X` to its bottom, the value `V` there */
  enum Frame[R <: Row, -X, +V]:
    case Bind[R <: Row, X, I, O, A, V](f: X => Cont0[R, I, O, A], rest: Frame[R, A, V]) extends Frame[R, X, V]
    case Map[R <: Row, X, A, V](f: X => A, rest: Frame[R, A, V]) extends Frame[R, X, V]
    case End[R <: Row, V]() extends Frame[R, V, V]

  /** THE FRAMES of a level of control: a value of `X` to the `$`'s `ret`, the answer `I` of the rest */
  enum CFrame[R <: Row, -X, +I]:
    case Return[R <: Row, X, I](ret: X => I) extends CFrame[R, X, I]
    case Bind[R <: Row, X, I, T, A](f: X => Cont0[Control[R], I, T, A], rest: CFrame[R, A, I]) extends CFrame[R, X, T]
    case Map[R <: Row, X, A, I](f: X => A, rest: CFrame[R, A, I]) extends CFrame[R, X, I]

  /** THE REST, `k`, as data: the levels an operation went out past, outermost first, each a level with the frames
   * inside it, the innermost over the rest `inner` — `k(y)` a `Resume`: the machine puts the levels back on the `Meta`
   * where it is resumed, outermost first, then runs `inner(y)` under them. Of each kind of level a case. Those with
   * effects outside them here; a `Reset` holding a level of control, its rest one of those, `ContC` */
  /** A REST GIVEN AN ERROR: a `k`'s levels put back, the error raised inside them — their finalizers run, their
   * recovers asked; `null`, raised where the rest is */
  trait Fails[+C]:
    def fail(e: Throwable): C

  enum Cont[-Y, T <: Row, B] extends (Y => B ! T) with Fails[B ! T]:
    case Under[Y, E[_], T <: Row, X, A, B](h: Stateless[E, T, A, B], k: Frame[E +: T, X, A],
        inner: Y => Cont0[E +: T, ?, ?, X], fails: Fails[Cont0[E +: T, ?, ?, X]] | Null) extends Cont[Y, T, B]
    case UnderState[Y, E[_], T <: Row, S, X, A, B](h: Stateful[E, T, S, A, B], s: S, k: Frame[E +: T, X, A],
        inner: Y => Cont0[E +: T, ?, ?, X], fails: Fails[Cont0[E +: T, ?, ?, X]] | Null) extends Cont[Y, T, B]
    case UnderView[Y, E[_], R <: Row, T <: Row, X, A](rm: Removed[E, R, T], k: Frame[R, X, A],
        inner: Y => Cont0[R, ?, ?, X], fails: Fails[Cont0[R, ?, ?, X]] | Null) extends Cont[Y, E +: T, A]
    /** a guard's level */
    case UnderGuard[Y, T <: Row, X, A, B](g: Guard[T, A, B], k: Frame[T, X, A], inner: Y => Cont0[T, ?, ?, X],
        fails: Fails[Cont0[T, ?, ?, X]] | Null) extends Cont[Y, T, B]
    /** a `$` of `reset`: a level of control in a program of effects */
    case UnderReset[Y, R <: Row, O, I, X](k: CFrame[R, X, I], inner: Y => Cont0[Control[R], I, O, X],
        fails: Fails[Cont0[Control[R], I, O, X]] | Null) extends Cont[Y, R, O]
    def apply(y: Y): B ! T = Resume(this, y)
    def fail(e: Throwable): B ! T = Fail(this, e)

  /** the rest's levels with control outside them: a `$` in control, or a program of effects in it — `k(y)` a program
   * of control, `ResumeC` */
  enum ContC[-Y, R <: Row, I, O, A] extends (Y => Cont0[Control[R], I, O, A]) with Fails[Cont0[Control[R], I, O, A]]:
    case UnderDollar[Y, R <: Row, I, O, X, Ii, Xi](k: CFrame[R, Xi, Ii],
        inner: Y => Cont0[Control[R], Ii, Cont0[Control[R], I, O, X], Xi],
        fails: Fails[Cont0[Control[R], Ii, Cont0[Control[R], I, O, X], Xi]] | Null) extends ContC[Y, R, I, O, X]
    case UnderLifted[Y, R <: Row, S, A, X](k: Frame[R, X, A], inner: Y => Cont0[R, ?, ?, X],
        fails: Fails[Cont0[R, ?, ?, X]] | Null) extends ContC[Y, R, S, S, A]
    def apply(y: Y): Cont0[Control[R], I, O, A] = ResumeC(this, y)
    def fail(e: Throwable): Cont0[Control[R], I, O, A] = FailC(this, e)

  /** a mask among a rest's levels */
  @tailrec private def masked(f: Fails[?] | Null): Boolean = f match
    case cn: Cont.UnderGuard[y, t, x, a, b] => cn.g.masked || masked(cn.fails)
    case cn: Cont.Under[y, e, t, x, a, b] => masked(cn.fails)
    case cn: Cont.UnderState[y, e, t, s, x, a, b] => masked(cn.fails)
    case cn: Cont.UnderView[y, e, r, t, x, a] => masked(cn.fails)
    case cn: Cont.UnderReset[y, r, o, i, x] => masked(cn.fails)
    case cn: ContC.UnderDollar[y, r, i, o, x, ii, xi] => masked(cn.fails)
    case cn: ContC.UnderLifted[y, r, s, a, x] => masked(cn.fails)
    case _ => false

  /** a mask left: a cancel asked under it fired, if no mask is left outside it — unwound from outside it */
  private def maskLeft[T <: Row, V, B, V2, Z](u: Meta.UnderGuard[T, V, B, V2, Z], v: V): Next[Z] =
    if cancelAsked(u.m) then Eff[T, Nothing, Nothing, V2, V2, Z](Raise(Cancelled), Frame.End(), u.m)
    else Eff(guarded(u.g.ret(v)), u.k, u.m)

  /** a cancel asked at the top, no mask between: true once — the ask taken */
  @tailrec private def cancelAsked(m: Meta[?, ?, ?]): Boolean = m match
    case t: Meta.Top[r, v, z] =>
      val asked = t.cancelAsked
      t.cancelAsked = false
      asked
    case u: Meta.UnderGuard[t, v, b, v2, z] => !u.g.masked && cancelAsked(u.m)
    case u: Meta.Under[e, t, v, b, v2, z] => cancelAsked(u.m)
    case u: Meta.UnderState[e, t, s, v, b, v2, z] => cancelAsked(u.m)
    case u: Meta.UnderView[e, r, t, v, v2, z] => cancelAsked(u.m)
    case _ => m.passed match
      case null => false
      case o => cancelAsked(o)

  /** the same, of a rest whose answers are its own */
  private def failedAny[R, X](f: Fails[Cont0[R, ?, ?, X]] | Null, e: Throwable): Cont0[R, ?, ?, X] = f match
    case null => Raise[R, Any, Any, X](e)
    case f: Fails[Cont0[R, ?, ?, X]] => f.fail(e)

  /** the program a rest's error starts: raised inside its levels, or where it is */
  private def failed[R, I, O, A](f: Fails[Cont0[R, I, O, A]] | Null, e: Throwable): Cont0[R, I, O, A] = f match
    case null => Raise(e)
    case f: Fails[Cont0[R, I, O, A]] => f.fail(e)

  /** WHAT THE TOP DOES with an operation come to it: none comes (`Pure`); given back with its rest, a step */
  enum Out[R <: Row, Z]:
    case None[Z]() extends Out[Pure, Z]
    case Given[R <: Row, Z](give: [E[_], Y] => (E[Y], In[E, R], Y => Z, Throwable => Z, () => Z) => Z)
        extends Out[R, Z]

  object Out:
    def none[Z]: Out[Pure, Z] = None()

  /** THE LEVELS in force: what takes a level's value `V` at its bottom, then the frames outside it, and the levels
   * outside those; each with the row of the code inside it, `R`. Two families, by the code inside: of effects
   * (`MetaE`), of control (`MetaC`) — each level's outside of the kind its frames there are. `UnderState`'s state a
   * register */
  sealed abstract class Meta[R <: Row, -V, +Z]:
    /** the level outside, if an operation passes this one, its row the same: a `$`, a program of effects in control */
    def passed: Meta[R, Nothing, Z] | Null
  sealed abstract class MetaE[R <: Row, -V, +Z] extends Meta[R, V, Z]:
    def passed: Meta[R, Nothing, Z] | Null = null
  sealed abstract class MetaC[R <: Row, -O, +Z] extends Meta[R, O, Z]
  object Meta:
    final class Top[R <: Row, V, Z](val k: V => Z, val fail: Throwable => Z, val out: Out[R, Z]) extends MetaE[R, V, Z]:
      /** a cancel asked under a mask: fired when the program leaves its last */
      var cancelAsked = false
    final class Under[E[_], T <: Row, V, B, V2, Z](val h: Stateless[E, T, V, B], val k: Frame[T, B, V2],
        val m: MetaE[T, V2, Z]) extends MetaE[E +: T, V, Z]:
      /** an abort's program, to run here when the levels inside are left — their finalizers run first */
      var pending: (B ! T) | Null = null
    /** a guard's level: the row the same in it */
    final class UnderGuard[T <: Row, V, B, V2, Z](val g: Guard[T, V, B], val k: Frame[T, B, V2],
        val m: MetaE[T, V2, Z]) extends MetaE[T, V, Z]:
      override def passed = m
    final class UnderState[E[_], T <: Row, S, V, B, V2, Z](val h: Stateful[E, T, S, V, B], var s: S,
        val k: Frame[T, B, V2], val m: MetaE[T, V2, Z]) extends MetaE[E +: T, V, Z]
    final class UnderView[E[_], R <: Row, T <: Row, V, V2, Z](val rm: Removed[E, R, T], val k: Frame[E +: T, V, V2],
        val m: MetaE[E +: T, V2, Z]) extends MetaE[R, V, Z]
    /** a program of effects in control: its value to the frames of control outside it */
    final class Lifted[R <: Row, A, S, Z](val k: CFrame[R, A, S], val m: MetaC[R, S, Z]) extends MetaE[R, A, Z]:
      override def passed = m
    /** the top of a program of control: its answer */
    final class TopC[O, Z](val k: O => Z, val fail: Throwable => Z) extends MetaC[Pure, O, Z]:
      def passed = null
    /** the `$` of a `reset`: the answer `O` at its bottom, to the frames of effects outside it */
    final class Reset[R <: Row, O, V2, Z](val k: Frame[R, O, V2], val m: MetaE[R, V2, Z]) extends MetaC[R, O, Z]:
      def passed = m
    /** a `$` in control: its answer, a program of control, run by the frames outside it */
    final class Dollar[R <: Row, I, O, X, Z](val k: CFrame[R, X, I], val m: MetaC[R, O, Z])
        extends MetaC[R, Cont0[Control[R], I, O, X], Z]:
      def passed = m

  // THE STATES between the loops

  /** what a loop hands on: the other loop's state, the way out of an operation, or the answer */
  sealed abstract class Next[+Z]
  final class Eff[R <: Row, I, O, A, V, Z](val c: Cont0[R, I, O, A], val k: Frame[R, A, V], val m: MetaE[R, V, Z])
      extends Next[Z]
  final class Ctl[R <: Row, I, O, A, Z](val c: Cont0[Control[R], I, O, A], val k: CFrame[R, A, I],
      val m: MetaC[R, O, Z]) extends Next[Z]
  /** on the way out to a handler, at a level of control: the rest so far a program of control */
  final class WayC[E[_], Y, R <: Row, I, O, A, Z](val op: E[Y], val in: In[E, R],
      val g: Y => Cont0[Control[R], I, O, A], val gf: Fails[Cont0[Control[R], I, O, A]] | Null, val k: CFrame[R, A, I],
      val m: MetaC[R, O, Z]) extends Next[Z]
  /** on the way out to a handler, at a level of effects */
  final class WayE[E[_], Y, R <: Row, I, O, A, V, Z](val op: E[Y], val in: In[E, R],
      val g: Y => Cont0[R, I, O, A], val gf: Fails[Cont0[R, I, O, A]] | Null, val k: Frame[R, A, V],
      val m: MetaE[R, V, Z]) extends Next[Z]
  final class Done[Z](val z: Z) extends Next[Z]

  /** the loops in turn, to the answer */
  @tailrec def drive[Z](n: Next[Z]): Z = n match
    case e: Eff[r, i, o, a, v, z] => drive(eval(e.c, e.k, e.m))
    case c: Ctl[r, i, o, a, z] => drive(ceval(c.c, c.k, c.m))
    case w: WayE[e, y, r, i, o, a, v, z] => drive(outE(w.op, w.in, w.g, w.gf, w.k, w.m))
    case w: WayC[e, y, r, i, o, a, z] => drive(outC(w.op, w.in, w.g, w.gf, w.k, w.m))
    case d: Done[z] => d.z

  /** A CALL OF USER CODE: its program, or `Raise` of what it threw — the `try` at the call, never around the loop */
  private inline def guarded[R, I, O, A](inline c: Cont0[R, I, O, A]): Cont0[R, I, O, A] =
    try c catch case e: Throwable => raised(e)

  /** A WAY OUT not an error: an abort to a handler's level, a cancel — unwound as one, no guard recovering it.
   * Never thrown: a value of the machine's */
  sealed abstract class Exit extends Throwable(null, null, false, false)
  /** an abort: its program pending at `level`, run there when the levels inside it are left */
  final class AbortTo(val level: AnyRef) extends Exit
  /** a cancel, from a step */
  object Cancelled extends Exit

  /** what user code threw, a `Raise` — a fatal error thrown on: the handler at each call one call, kept small */
  private def raised[R, I, O, A](e: Throwable): Cont0[R, I, O, A] = if NonFatal(e) then Raise(e) else throw e

  /** AN ERROR RAISED: the frames of each level dropped, the levels unwound innermost first — a state's register gone
   * with its level — to the top, its answer to it */
  @tailrec private def unwind[R <: Row, V, Z](e: Throwable, m: MetaE[R, V, Z] | MetaC[R, V, Z]): Next[Z] = m match
    case u: Meta.Under[e, t, v, b, v2, z] => e match
      case to: AbortTo if to.level eq u => u.pending match
        case null => unwind(e, u.m)
        case p => Eff[t, Nothing, Nothing, b, v2, Z](p, u.k, u.m)
      case _ => unwind(e, u.m)
    case u: Meta.UnderGuard[t, v, b, v2, z] =>
      val recovered: (b ! t) | Null = e match
        case _: Exit => null
        case _ => try u.g.recover(e) catch case e2: Throwable => raised[t, Nothing, Nothing, b](e2)
      if recovered != null then Eff[t, Nothing, Nothing, b, v2, Z](recovered, u.k, u.m)
      else u.g.finalizer match
        case null => unwind(e, u.m)
        // the finalizer where the guard was, the frames outside it dropped; then the unwinding on
        case fin => Eff[t, Nothing, Nothing, v2, v2, Z](Bind(fin, (_: Unit) => Raise[t, Nothing, Nothing, v2](e)),
          Frame.End(), u.m)
    case u: Meta.UnderState[e, t, s, v, b, v2, z] => unwind(e, u.m)
    case u: Meta.UnderView[e, r, t, v, v2, z] => unwind(e, u.m)
    case l: Meta.Lifted[r, a, s, z] => unwind(e, l.m)
    case r: Meta.Reset[r, o, v2, z] => unwind(e, r.m)
    case d: Meta.Dollar[r, i, o, x, z] => unwind(e, d.m)
    case t: Meta.Top[r, v, z] => Done(t.fail(e))
    case t: Meta.TopC[o, z] => Done(t.fail(e))

  /** a value at the bottom of a level of effects, not a handler's: a view's, a program of effects' in control, the
   * top's */
  private def bottomE[R <: Row, V, Z](v: V, m: MetaE[R, V, Z]): Next[Z] = m match
    case t: Meta.Top[r, v, z] => Done(t.k(v))
    case u: Meta.Under[e, t, v, b, v2, z] => Eff(guarded(u.h.ret(v)), u.k, u.m)
    case u: Meta.UnderState[e, t, s, v, b, v2, z] => Eff(guarded(u.h.ret(u.s, v)), u.k, u.m)
    case u: Meta.UnderView[e, r, t, v, v2, z] => Eff[e +: t, Nothing, Nothing, v, v2, Z](Return(v), u.k, u.m)
    case l: Meta.Lifted[r, a, s, z] => Ctl[r, s, s, a, Z](Return(v), l.k, l.m)
    case u: Meta.UnderGuard[t, v, b, v2, z] => if u.g.masked then maskLeft(u, v) else Eff(guarded(u.g.ret(v)), u.k, u.m)

  /** an answer at the bottom of a level of control: a `$`'s — run by the frames outside it — or the top's */
  private def bottomC[R <: Row, O, Z](o: O, m: MetaC[R, O, Z]): Next[Z] = m match
    case t: Meta.TopC[o, z] => try Done(t.k(o)) catch case NonFatal(e) => Done(t.fail(e))
    case r: Meta.Reset[r, o, v2, z] => Eff[r, Nothing, Nothing, o, v2, Z](Return(o), r.k, r.m)
    case d: Meta.Dollar[r, i, o2, x, z] => Ctl[r, i, o2, x, Z](o, d.k, d.m)

  // LOOKING UP A HANDLER

  /** the handler of `E` at `in`, found by the path, nothing taken: answering in place, its level with a state, or an
   * aborting one's — else `null`, one taking `k`, to be made. `$`s passed, their row the code's in them */
  type Level[E[_], Z] = Answering[E, ?, ?, ?] | Meta.UnderState[E, ?, ?, ?, ?, ?, ?] |
    Meta.Under[E, ?, ?, ?, ?, ? <: Z] | Null

  @tailrec private def level[E[_], R <: Row, V, Z](in: In[E, R], m: Meta[R, V, Z]): Level[E, Z] =
    m match
    case u: Meta.Under[e, t, v, b, v2, z] => in match
      case In.Here() => u.h match
        case a: Answering[e4, t4, v4, b4] => a
        case _: Aborting[e4, t4, v4, b4] => u
        case _ => null
      case In.There(in) => level(in, u.m)
    case u: Meta.UnderState[e, t, s, v, b, v2, z] => in match
      case In.Here() => u
      case In.There(in) => level(in, u.m)
    case u: Meta.UnderView[e, r, t, v, v2, z] => level(Removed.relocate(in, u.rm), u.m)
    case _ => m.passed match
      case null => null
      case o => level[E, R, Nothing, Z](in, o)

  /** an operation, its handler FOUND by its path: answered in place, its value to its rest `g` at once — no node made
   * for it; an abort, the levels between dropped; else out to its handler, `forward`. Inlined into the loop of
   * effects: its calls of `eval` the loop's tail calls */
  private inline def found[E[_], Y, R <: Row, I, O, A, V, Z](op: E[Y], in: In[E, R], g: Y => Cont0[R, I, O, A],
      k: Frame[R, A, V], m: MetaE[R, V, Z]): Next[Z] = lookUp[E, R, V, Z](in, m) match
    case a: Answering[e4, t4, v4, b4] => eval(guarded(g(a.value[Y](op))), k, m)
    case null => outE(op, in, g, null, k, m)
    case u: Meta.UnderState[e2, t2, s2, v, b, v2, z] =>
      eval(guarded { val value = u.h.value[Y](u.s, op); u.s = u.h.next(u.s, op); g(value) }, k, m)
    case u: Meta.Under[e2, t2, v, b, v2, z] => u.h match
      case ab: Aborting[e4, t4, v4, b4] =>
        if guardsTo(m, u) then
          u.pending = guarded(ab.abort[Y](op))
          eval(Raise[R, I, O, A](AbortTo(u)), k, m)
        else eval[t2, Nothing, Nothing, b, v2, Z](guarded(ab.abort[Y](op)), u.k, u.m)
      case _ => outE(op, in, g, null, k, m)

  /** an operation answered by a state in place: its value to the rest `g`, the register moved on — none moved if
   * user code throws */
  private def stated[E[_], T <: Row, S, V, B, V2, Z, Y, R <: Row, I, O, A](u: Meta.UnderState[E, T, S, V, B, V2, Z],
      op: E[Y], g: Y => Cont0[R, I, O, A]): Cont0[R, I, O, A] =
    try
      val value = u.h.value[Y](u.s, op)
      u.s = u.h.next(u.s, op)
      g(value)
    catch case e: Throwable => raised(e)

  /** A LOOK-UP KEPT on the path: the handler answering in place, or the state's level, found from `at` — the same
   * levels, the same answer while `at` is where the operation is */
  final class Found[E[_]](val at: AnyRef, val level: Answering[E, ?, ?, ?] | Meta.UnderState[E, ?, ?, ?, ?, ?, ?])
      extends In.Cache[E]

  /** `level`, the path's memory first: from the same level, its handler at once */
  private def lookUp[E[_], R <: Row, V, Z](in: In[E, R], m: MetaE[R, V, Z]): Level[E, Z] = in.cache match
    case f: Found[e] if f.at eq m => f.level match
      case a: Answering[e4, t4, v4, b4] => a
      case u: Meta.UnderState[e2, t2, s2, v2, b, v3, z] => u
    case _ =>
      val l = level[E, R, V, Z](in, m)
      l match
        case a: Answering[e4, t4, v4, b4] => in.cache = Found[E](m, a)
        case u: Meta.UnderState[e2, t2, s2, v2, b, v3, z] => in.cache = Found[E](m, u)
        case _ => ()
      l

  /** a guard among the levels from `m` to `to`: an abort to `to` unwinding them, not a jump */
  @tailrec private def guardsTo(m: Meta[?, ?, ?] | Null, to: AnyRef): Boolean = m match
    case null => false
    case _ if m eq to => false
    case _: Meta.UnderGuard[t, v, b, v2, z] => true
    case u: Meta.Under[e, t, v, b, v2, z] => guardsTo(u.m, to)
    case u: Meta.UnderState[e, t, s, v, b, v2, z] => guardsTo(u.m, to)
    case u: Meta.UnderView[e, r, t, v, v2, z] => guardsTo(u.m, to)
    case n: Meta[?, ?, ?] => guardsTo(n.passed, to)

  /** a clause of a handler taking `k`, at its level: `Deep`'s with it, `Translating`'s program then it */
  private def clause[E[_], T <: Row, A, B, Y](h: Stateless[E, T, A, B], op: E[Y], k: Cont[Y, T, B]): B ! T =
    try h match
      case d: Deep[e, t, a, b] => d[Y](op, k)
      case tr: Translating[e, t, a, b] => tr[Y](op).flatMap(k)
      case a: Answering[e, t, a1, b] => k(a.value[Y](op))
      case ab: Aborting[e, t, a, b] => ab.abort[Y](op)
    catch case e: Throwable => raised(e)

  // THE LOOP OF EFFECTS

  @tailrec def eval[R <: Row, I, O, A, V, Z](c: Cont0[R, I, O, A], k: Frame[R, A, V], m: MetaE[R, V, Z]): Next[Z] = c match
    case Return(a) => k match
      case Frame.Bind(f, rest) => eval(guarded(f(a)), rest, m)
      case mf: Frame.Map[r, x, a1, v] => mf.rest match
        // two maps: both functions at once — no value between them made into a program
        case mf2: Frame.Map[r2, x2, a2, v2] => eval(guarded(Return(mf2.f(mf.f(a)))), mf2.rest, m)
        case _ => eval(guarded(Return(mf.f(a))), mf.rest, m)
      case Frame.End() => m match
        case t: Meta.Top[r, v, z] => Done(t.k(a))
        case u: Meta.Under[e, t, v, b, v2, z] => eval[t, Nothing, Nothing, b, v2, Z](guarded(u.h.ret(a)), u.k, u.m)
        case u: Meta.UnderState[e, t, s, v, b, v2, z] => eval[t, Nothing, Nothing, b, v2, Z](guarded(u.h.ret(u.s, a)), u.k, u.m)
        case u: Meta.UnderView[e, r, t, v, v2, z] => eval[e +: t, Nothing, Nothing, v, v2, Z](Return(a), u.k, u.m)
        case l: Meta.Lifted[r, a2, s, z] => Ctl[r, s, s, a2, Z](Return(a), l.k, l.m)
        case u: Meta.UnderGuard[t, v, b, v2, z] =>
          if u.g.masked then maskLeft(u, a) else eval[t, Nothing, Nothing, b, v2, Z](guarded(u.g.ret(a)), u.k, u.m)
    case b: Bind[r, i, t, o, x, a] => b.m match
      case Return(a) => eval(guarded(b.f(a)), k, m) // a value bound: on at once, no frame
      // a handler's level resumed, bound: entered here at once, the bind its outer frame; its rest a value, nothing
      // after it — the handler's `ret`, no level entered for it
      case rs: Resume[y, t2, s, b2] => rs.c match
        case cn: Cont.Under[y2, e, t3, x, a1, b3] => cn.k match
          case Frame.End() =>
            val body: Cont0[e +: t3, ?, ?, x] =
              try cn.inner(rs.y) catch case err: Throwable => raised[e +: t3, Any, Any, x](err)
            body match
              case Return(v) =>
                val ret: b3 ! t3 = guarded(cn.h.ret(v))
                ret match
                  case Return(bv) => eval(guarded(b.f(bv)), k, m)
                  case r => eval(r, Frame.Bind(b.f, k), m)
              case _ => eval(body, cn.k, Meta.Under(cn.h, Frame.Bind(b.f, k), m))
          case _ => eval(guarded(cn.inner(rs.y)), cn.k, Meta.Under(cn.h, Frame.Bind(b.f, k), m))
        case _ => eval(b.m, Frame.Bind(b.f, k), m)
      case _ => eval(b.m, Frame.Bind(b.f, k), m)
    case mp: Map[r, i, o, x, a] => mp.m match
      case Return(a) => eval(guarded(Return(mp.f(a))), k, m)
      case _ => eval(mp.m, Frame.Map(mp.f, k), m)
    // an operation of the row's head: its handler the nearest level, if that is at the head — no path walked; with no
    // rest, answered in place, its value straight into the frame on top
    case p: PerformHere[e, t, s, x] => m match
      case u: Meta.Under[e2, t2, v, b, v2, z] => u.h match
        case a: Answering[e4, t4, v4, b4] => k match
          case Frame.Bind(f, rest) => eval(guarded(f(a.value[x](p.op))), rest, m)
          case Frame.Map(f, rest) => eval(guarded(Return(f(a.value[x](p.op)))), rest, m)
          case Frame.End() => eval(guarded(Return[R, s, x](a.value[x](p.op))), k, m)
        case h => eval[t2, Nothing, Nothing, b, v2, Z](clause(h, p.op, Cont.Under(h, k, (y: x) => Return[R, s, x](y), null)),
          u.k, u.m)
      case u: Meta.UnderState[e2, t2, s2, v, b, v2, z] => k match
        case Frame.Bind(f, rest) => eval(guarded { val value = u.h.value[x](u.s, p.op); u.s = u.h.next(u.s, p.op); f(value) }, rest, m)
        case Frame.Map(f, rest) => eval(guarded { val value = u.h.value[x](u.s, p.op); u.s = u.h.next(u.s, p.op); Return(f(value)) }, rest, m)
        case Frame.End() => eval(guarded { val value = u.h.value[x](u.s, p.op); u.s = u.h.next(u.s, p.op); Return[R, s, x](value) }, k, m)
      // seen from outside a handler of another effect, or the top: its place made, found the usual way
      case _ => found(p.op, In.Here[e, t](), (y: x) => Return[R, s, x](y), k, m)
    case ih: InjectHere[e, t, y, i, o, a] => m match
      case u: Meta.Under[e2, t2, v, b, v2, z] => u.h match
        case a: Answering[e4, t4, v4, b4] => eval(guarded(ih.g(a.value[y](ih.op))), k, m)
        case h => eval[t2, Nothing, Nothing, b, v2, Z](clause(h, ih.op, Cont.Under(h, k, ih.g, null)), u.k, u.m)
      case u: Meta.UnderState[e2, t2, s2, v, b, v2, z] =>
        eval(guarded { val value = u.h.value[y](u.s, ih.op); u.s = u.h.next(u.s, ih.op); ih.g(value) }, k, m)
      case _ => found(ih.op, In.Here[e, t](), ih.g, k, m)
    // an operation with its place, not the head: its handler found by its path
    case p: Perform[r, e, s, x] => found(p.op, p.in, (y: x) => Return[R, s, x](y), k, m)
    case op: Inject[r, e, y, i, o, a] => found(op.op, op.in, op.g, k, m)
    // a `k` resumed: its outermost level put back, its frames those inside it; the rest, a `Resume` of the inner levels
    // or `inner(y)` itself, under it
    case rs: Resume[y, t, s, b] => rs.c match
      case cn: Cont.Under[y2, e, t2, x, a, b2] => eval(guarded(cn.inner(rs.y)), cn.k, Meta.Under(cn.h, k, m))
      case cn: Cont.UnderState[y2, e, t2, s2, x, a, b2] =>
        eval(guarded(cn.inner(rs.y)), cn.k, Meta.UnderState(cn.h, cn.s, k, m))
      case cn: Cont.UnderView[y2, e, r, t2, x, a] => eval(guarded(cn.inner(rs.y)), cn.k, Meta.UnderView(cn.rm, k, m))
      case cn: Cont.UnderReset[y2, r, o, i, x] => Ctl(guarded(cn.inner(rs.y)), cn.k, Meta.Reset(k, m))
      case cn: Cont.UnderGuard[y2, t2, x, a, b2] => Eff(guarded(cn.inner(rs.y)), cn.k, Meta.UnderGuard(cn.g, k, m))
    // a handler entered: its level, the frames outside it those here; its body on its own frames
    case h: Handle[e, t, s, x, a, b] => eval(h.body, h.k, Meta.Under(h.h, k, m))
    case h: HandleState[e, t, st, s, x, a, b] => eval(h.body, h.k, Meta.UnderState(h.h, h.s, k, m))
    case v: View[e, r, t, s, x, a] => eval(v.body, v.k, Meta.UnderView(v.rm, k, m))
    case Defer(p) => eval(guarded(p()), k, m)

    case l: Lower[r, s, o, s2] => lowered(l, k, m)
    case g: Guarded[t, s, x, a, b] => Eff(g.body, g.k, Meta.UnderGuard(g.g, k, m))
    case fl: Fail[t, s, b] => failIn(fl, k, m)
    case Raise(e) => unwind(e, m)

  /** a `k` failed: its outermost level put back, then the error raised inside the rest — its own levels, or here */
  private def failIn[T <: Row, S, B, V, Z](fl: Fail[T, S, B], k: Frame[T, B, V], m: MetaE[T, V, Z]): Next[Z] =
    fl.c match
      case cn: Cont.Under[y, e, t2, x, a, b2] =>
        Eff(failedAny(cn.fails, fl.e), cn.k, Meta.Under(cn.h, k, m))
      case cn: Cont.UnderState[y, e, t2, s, x, a, b2] =>
        Eff(failedAny(cn.fails, fl.e), cn.k, Meta.UnderState(cn.h, cn.s, k, m))
      case cn: Cont.UnderView[y, e, r, t2, x, a] =>
        Eff(failedAny(cn.fails, fl.e), cn.k, Meta.UnderView(cn.rm, k, m))
      case cn: Cont.UnderGuard[y, t2, x, a, b2] =>
        Eff(failedAny(cn.fails, fl.e), cn.k, Meta.UnderGuard(cn.g, k, m))
      case cn: Cont.UnderReset[y, r, o, i, x] => Ctl(failed(cn.fails, fl.e), cn.k, Meta.Reset(k, m))

  /** a `k` failed in control: its outermost level put back, then the error raised inside the rest */
  private def failInC[R <: Row, I, O, A, Z](fc: FailC[R, I, O, A], k: CFrame[R, A, I], m: MetaC[R, O, Z]): Next[Z] =
    fc.c match
      case cn: ContC.UnderDollar[y, r2, i2, o2, x, ii, xi] => Ctl(failed(cn.fails, fc.e), cn.k, Meta.Dollar(k, m))
      case cn: ContC.UnderLifted[y, r2, s, a2, x] =>
        Eff(failedAny(cn.fails, fc.e), cn.k, Meta.Lifted[R, A, I, Z](k, m))

  /** a view entered: its level */
  private def viewed[E[_], R <: Row, T <: Row, S, X, A, V, Z](v: View[E, R, T, S, X, A], k: Frame[E +: T, A, V],
      m: MetaE[E +: T, V, Z]): Next[Z] = Eff(v.body, v.k, Meta.UnderView(v.rm, k, m))

  /** a `reset`: its `$` a level, its body control on its own frames, the rest the identity */
  private def lowered[R <: Row, S, O, S2, V, Z](l: Lower[R, S, O, S2], k: Frame[R, O, V], m: MetaE[R, V, Z]): Next[Z] =
    Ctl(l.c, CFrame.Return((x: S) => x), Meta.Reset(k, m))

  // THE LOOP OF CONTROL

  @tailrec def ceval[R <: Row, I, O, A, Z](c: Cont0[Control[R], I, O, A], k: CFrame[R, A, I], m: MetaC[R, O, Z])
      : Next[Z] =
    c match
      case Return(a) => k match
        case CFrame.Bind(f, rest) => ceval(guarded(f(a)), rest, m)
        case mf: CFrame.Map[r, x, a1, i] => mf.rest match
          case mf2: CFrame.Map[r2, a2, a3, i2] => ceval(guarded(Return(mf2.f(mf.f(a)))), mf2.rest, m)
          case _ => ceval(guarded(Return(mf.f(a))), mf.rest, m)
        case CFrame.Return(r) => m match
          case d: Meta.Dollar[r2, i, o, x, z] => ceval[r2, i, o, x, Z](guarded(r(a)), d.k, d.m)
          case _ => try bottomC(r(a), m) catch case NonFatal(e) => unwind(e, m)
      case Bind(c1, f) => c1 match
        /** a value bound: on at once, no frame */
        case Return(a) => ceval(guarded(f(a)), k, m)
        /** a `k` resumed and bound, its rest to the `$` maps only: the `$`'s `ret` of its value here, no level
         * entered — a value at once, straight into the function, no frame for the bind */
        case r: Continue[r0, a1, i1, o1, b1] => r.k match
          case CFrame.Return(ret) => guarded(ret(r.a)) match
            case Return(b) => ceval(guarded(f(b)), k, m)
            case c2 => ceval(c2, CFrame.Bind(f, k), m)
          case _ => ceval(c1, CFrame.Bind(f, k), m)
        case _ => ceval(c1, CFrame.Bind(f, k), m)
      case Map(c1, f) => ceval(c1, CFrame.Map(f, k), m)
      /** the body given `k`, its answer to the `$` — out of the loop for the `try`, the `$` taking it a state */
      case s: Shift0[r, i1, o1, x1, o, a] =>
        try
          val o: O = s.f(x => Continue[r, A, i1, o1, x1](k, x))
          m match
            case d: Meta.Dollar[r2, i, o2, x, z] => Ctl[r2, i, o2, x, Z](o, d.k, d.m)
            case _ => bottomC(o, m)
        catch case NonFatal(e) => unwind(e, m)
      case Dollar(body, ret) => ceval(body, CFrame.Return(ret), Meta.Dollar(k, m))
      case Defer(t) => ceval(guarded(t()), k, m)
      case Raise(e) => unwind(e, m)
      case Continue(k1, a) => k1 match
        /** the rest to a value at once, maps only: the `$`'s `ret` of it, no level entered */
        case CFrame.Return(r) => ceval(guarded(r(a)), k, m)
        case mf: CFrame.Map[r, x, a1, i] => mf.rest match
          case CFrame.Return(r) => ceval(guarded(r(mf.f(a))), k, m)
          case _ => ceval(guarded(Return(mf.f(a))), mf.rest, Meta.Dollar(k, m))
        case _ => ceval(Return(a), k1, Meta.Dollar(k, m))
      /** a program of effects in control: its level, its value to the frames here */
      case l: Lift[r, i0, o0, s, a] => Eff(l.p, Frame.End(), Meta.Lifted[R, A, I, Z](k, m))
      /** the levels of a `k` with control outside them put back */
      case fc: FailC[r, i, o, a] => failInC(fc, k, m)
      case rc: ResumeC[y, r, i, o, a] => rc.c match
        case cn: ContC.UnderDollar[y2, r2, i2, o2, x, ii, xi] =>
          ceval(guarded(cn.inner(rc.y)), cn.k, Meta.Dollar(k, m))
        case cn: ContC.UnderLifted[y2, r2, s, a2, x] =>
          Eff(guarded(cn.inner(rc.y)), cn.k, Meta.Lifted[R, A, I, Z](k, m))

  // OUT TO A HANDLER TAKING `k`

  /** an operation ON ITS WAY OUT to a handler taking `k`, past levels of effects: each level passed put around its
   * rest `g`, a `Cont`; at the handler's level, its clause with it — the state there; at a program of effects in
   * control, on past the levels of control (`WayC`); at the top, given back. Each step a tail call */
  @tailrec private def outE[E[_], Y, R <: Row, I, O, A, V, Z](op: E[Y], in: In[E, R], g: Y => Cont0[R, I, O, A],
      gf: Fails[Cont0[R, I, O, A]] | Null, k: Frame[R, A, V], m: MetaE[R, V, Z]): Next[Z] = m match
    case u: Meta.Under[e, t, v, b, v2, z] => in match
      case In.Here() => Eff(clause(u.h, op, Cont.Under(u.h, k, g, gf)), u.k, u.m)
      case In.There(in) =>
        val c = Cont.Under(u.h, k, g, gf)
        outE(op, in, c, c, u.k, u.m)
    case u: Meta.UnderState[e, t, s, v, b, v2, z] => in match
      case In.Here() => Eff(stated(u, op, g), k, m)
      case In.There(in) =>
        val c = Cont.UnderState(u.h, u.s, k, g, gf)
        outE(op, in, c, c, u.k, u.m)
    case u: Meta.UnderView[e, r, t, v, v2, z] =>
      val c = Cont.UnderView(u.rm, k, g, gf)
      outE(op, Removed.relocate(in, u.rm), c, c, u.k, u.m)
    case u: Meta.UnderGuard[t, v, b, v2, z] =>
      val c = Cont.UnderGuard(u.g, k, g, gf)
      outE(op, in, c, c, u.k, u.m)
    case l: Meta.Lifted[r, a, s, z] =>
      val c = ContC.UnderLifted[Y, r, s, a, A](k, g, gf)
      WayC[E, Y, r, s, s, a, Z](op, in, c, c, l.k, l.m)
    case t: Meta.Top[r, v, z] => t.out match
      case _: Out.None[z2] => In.absurd(in)
      case Out.Given(give) =>
        lazy val back: z = give(op, in, (y: Y) => drive[z](Eff[R, I, O, A, V, z](guarded(g(y)), k, t)),
          (e: Throwable) => drive[z](Eff[R, I, O, A, V, z](failed(gf, e), k, t)),
          // a cancel: unwound from inside k's levels; under a mask, kept for its exit — the step given back
          () => if masked(gf) then { t.cancelAsked = true; back }
                else drive[z](Eff[R, I, O, A, V, z](failed(gf, Cancelled), k, t)))
        Done(back)

  /** on the way out past levels of control: a `$` put around the rest, a `ContC`; at a `reset`'s, on past the levels
   * of effects outside it (`WayE`) */
  @tailrec private def outC[E[_], Y, R <: Row, I, O, A, Z](op: E[Y], in: In[E, R],
      g: Y => Cont0[Control[R], I, O, A], gf: Fails[Cont0[Control[R], I, O, A]] | Null, k: CFrame[R, A, I],
      m: MetaC[R, O, Z]): Next[Z] = m match
    case d: Meta.Dollar[r, i, o, x, z] =>
      val c = ContC.UnderDollar[Y, r, i, o, x, I, A](k, g, gf)
      outC(op, in, c, c, d.k, d.m)
    case r: Meta.Reset[r, o, v2, z] => {
      val c = Cont.UnderReset[Y, r, o, I, A](k, g, gf)
      WayE[E, Y, r, Nothing, Nothing, o, v2, Z](op, in, c, c, r.k, r.m) }
    case _: Meta.TopC[o, z] => In.absurd(in)
