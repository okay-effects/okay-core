package okay.one

import scala.util.Random
import okay.one.Cont.*
import okay.one.Handlers.*
import okay.one.handler.*
import okay.one.row.*
import okay.one.machine.Machine
import okay.one.std.{Shift, Shifts}
import okay.one.step.Step
import okay.one.step.Step.*

/**
 * THE MACHINE AGAINST ONE REFERENCE: random programs of the whole monad run by the machine — with all it does for
 * speed — and by its meaning in continuation-passing style, each node by its rule, nothing clever: the same answers
 * required. Effects: an operation given to the handlers of its row with its rest; a handler the delimiter of its
 * program's rest. Control: shift0 and `$` as in the calculus, the effects in it answered by the handlers around.
 */
class TestReference extends munit.FunSuite:
  val Digest = -603317515

  // THE MEANING

  /** A PROGRAM'S NORMAL FORM: its value, or its first operation not handled in it — its place in the row and its rest */
  enum Norm[R <: Row, +A]:
    case Done(a: A)
    case Op[R <: Row, A, E[_], X](op: E[X], in: In[E, R], k: X => Norm[R, A]) extends Norm[R, A]

  /** A PROGRAM'S NORMAL FORM WITH ITS ANSWERS: as `Norm`; or its first shift0 not under a `$` in it — its path to the
   * `$`, its body, its rest. The answers Asai–Kameyama's: the rest's `I` what the shift0's `k` gives, the shift0's
   * what its body makes */
  enum NormC[P <: Row, I, +O, +A]:
    case Done[P <: Row, S, A](a: A) extends NormC[P, S, S, A]
    case Op[P <: Row, I, O, A, E[_], X](op: E[X], in: In[E, P], k: X => NormC[P, I, O, A]) extends NormC[P, I, O, A]
    case Shifted[P <: Row, T <: Row, I1, O1, X1, Y, I, O, A](at: Nearest[T, P], f: (Y => Cont[T, I1, O1, X1]) => O,
        k: Y => NormC[P, I, Cont[T, I1, O1, X1], A]) extends NormC[P, I, O, A]

  /** a program to its normal form, each node by its rule — a handler the textbook's: its body's normal form, an
   * operation of its own given to it with the rest under it again, one of another's going out past it, a shift0
   * going out past it with the handler in its rest, the state as it is; a `$` the shift0's, its rest the `$` put back,
   * its body's answer run outside */
  def normC[P <: Row, I, O, A](p: Cont[P, I, O, A]): NormC[P, I, O, A] = p match
    case Return(a) => NormC.Done(a)
    case b: Bind[r, i, t, o, x, a] => bindC(normC(b.m), (x: x) => normC(b.f(x)))
    case mp: Cont.Map[r, i, o, x, a] => bindC(normC(mp.m), (x: x) => NormC.Done(mp.f(x)))
    case df: Defer[r, i, o, a] => normC(df.c())
    case pf: Perform[r, e, s, x] => NormC.Op(pf.op, pf.in, (x: x) => NormC.Done(x))
    case ph: PerformHere[e, t, s, x] => NormC.Op(ph.op, In.Here[e, t](), (x: x) => NormC.Done(x))
    case ij: Inject[r, e, y, i, o, a] => NormC.Op(ij.op, ij.in, (y: y) => normC(ij.g(y)))
    case ih: InjectHere[e, t, y, i, o, a] => NormC.Op(ih.op, In.Here[e, t](), (y: y) => normC(ih.g(y)))
    case h: Handle[e, t, x, i, a, j, o, b] => handledC(h.h, framesC(normC(h.body), h.k))
    case h: HandleState[e, t, st, x, i, a, j, o, b] => statedC(h.h, h.s, framesC(normC(h.body), h.k))
    case h: HandleK[e, t, x, i, a, b] => effects(handled(h.h, frames(norm(h.body), h.k)))
    case v: View[e, r, t, x, i, a, j, o] => viewedC(v.rm, framesC(normC(v.body), v.k))
    case s: Shift0[r, t, i1, o1, x1, o, a] => NormC.Shifted(s.at, s.f, (y: a) => NormC.Done(y))
    // a shift0 and its rest: the shift0 bound to it
    case s: Shift0K[r, t, i1, o1, x1, o, a, i, b] => NormC.Shifted(s.at, s.f, (y: a) => normC(s.g(y)))
    case d: Dollar[t, i, i2, o2, b, a] => dollarC(normC(d.body), d.ret)
    case l: Lower[r, s, o] => lowerC(normC(l.c))
    case l: Lift[r, s, a] => effects(norm(l.p))
    case _ => throw IllegalStateException("not a node of a program written")

  /** a program of effects: its normal form, no shift0 in it */
  def norm[R <: Row, I, O, A](p: Cont[R, I, O, A]): Norm[R, A] = toNorm(normC(p))

  def toNorm[P <: Row, I, O, A](n: NormC[P, I, O, A]): Norm[P, A] = n match
    case NormC.Done(a) => Norm.Done(a)
    case o: NormC.Op[p, i, o, a, e, x] => Norm.Op(o.op, o.in, (x: x) => toNorm(o.k(x)))
    case _: NormC.Shifted[p, t, i1, o1, x1, y, i, o, a] => throw IllegalStateException("a shift0 with no $ around: not written")

  /** a program of effects' normal form at any answer: none of its own */
  def effects[P <: Row, S, A](n: Norm[P, A]): NormC[P, S, S, A] = n match
    case Norm.Done(a) => NormC.Done(a)
    case o: Norm.Op[p, a, e, x] => NormC.Op(o.op, o.in, (x: x) => effects(o.k(x)))

  def bind[R <: Row, X, A](n: Norm[R, X], f: X => Norm[R, A]): Norm[R, A] = n match
    case Norm.Done(x) => f(x)
    case o: Norm.Op[r, x, e, y] => Norm.Op(o.op, o.in, (y: y) => bind(o.k(y), f))

  def bindC[P <: Row, X, I, T, O, A](n: NormC[P, T, O, X], f: X => NormC[P, I, T, A]): NormC[P, I, O, A] = n match
    case NormC.Done(x) => f(x)
    case o: NormC.Op[p, t, o, x, e, y] => NormC.Op(o.op, o.in, (y: y) => bindC(o.k(y), f))
    case s: NormC.Shifted[p, t2, i1, o1, x1, y, t, o, x] => NormC.Shifted(s.at, s.f, (y: y) => bindC(s.k(y), f))

  def frames[R <: Row, X, I, A, J](n: Norm[R, X], k: Machine.Frame[R, X, I, A, J]): Norm[R, A] = k match
    case b: Machine.Frame.Bind[r, x, i, t, a, v, j] => frames(bind(n, (x: x) => norm(b.f(x))), b.rest)
    case mp: Machine.Frame.Map[r, x, i, a, v, j] => frames(bind(n, (x: x) => Norm.Done(mp.f(x))), mp.rest)
    case Machine.Frame.End() => n

  def framesC[P <: Row, X, I, O, V, J](n: NormC[P, I, O, X], k: Machine.Frame[P, X, I, V, J]): NormC[P, J, O, V] = k match
    case b: Machine.Frame.Bind[p, x, i, t, a, v, j] => framesC(bindC(n, (x: x) => normC(b.f(x))), b.rest)
    case mp: Machine.Frame.Map[p, x, i, a, v, j] => framesC(bindC(n, (x: x) => NormC.Done(mp.f(x))), mp.rest)
    case Machine.Frame.End() => n

  /** a normal form a program again: its value, or its operation bound to the rest */
  def reify[R <: Row, A](n: Norm[R, A]): A ! R = n match
    case Norm.Done(a) => Return(a)
    case o: Norm.Op[r, a, e, x] => Inject(o.op, o.in, (x: x) => reify(o.k(x)))

  def reifyC[P <: Row, I, O, A](n: NormC[P, I, O, A]): Cont[P, I, O, A] = n match
    case NormC.Done(a) => Return(a)
    case o: NormC.Op[p, i, o, a, e, x] => Inject(o.op, o.in, (x: x) => reifyC(o.k(x)))
    case s: NormC.Shifted[p, t, i1, o1, x1, y, i, o, a] => Bind(Shift0(s.at, s.f), (y: y) => reifyC(s.k(y)))

  def handled[E[_], T <: Row, A, B](h: Stateless[E, T, A, B], n: Norm[E +: T, A]): Norm[T, B] = n match
    case Norm.Done(a) => norm(h.ret(a))
    case o: Norm.Op[r, a, f, x] => o.in match
      case _: In.Here[f2, t] => h match
        case h: Answering[E, T, A, B] => handled(h, o.k(h.value(o.op)))
        case h: Aborting[E, T, A, B] => norm(h.abort(o.op))
        case h: Translating[E, T, A, B] => bind(norm(h(o.op)), (x: x) => handled(h, o.k(x)))
        case h: Linear[E, T, A, B] => norm(h[Nothing, Nothing, x](o.op, (x: x) => reify(handled(h, o.k(x)))))
        case h: Deep[E, T, A, B] => norm(h(o.op, (x: x) => reify(handled(h, o.k(x)))))
      case th: In.There[f2, g, t] => Norm.Op(o.op, th.in, (x: x) => handled(h, o.k(x)))
      // a place by key: the handler's own, as the head; else past it, the same place
      case at: In.At[f2, r2] => h.at(at) match
        case null => Norm.Op(o.op, at.again, (x: x) => handled(h, o.k(x)))
        case s: In.Same[E, f] => s match
          case In.Same.Refl() => handled(h, Norm.Op(o.op, In.Here[f, T](), o.k))

  /** a handler leaving the rest, at the answers through it: a shift0 going out past it takes it into its rest */
  def handledC[E[_], T <: Row, J, O, A, B](h: Passing[E, T, A, B], n: NormC[E +: T, J, O, A]): NormC[T, J, O, B] = n match
    case NormC.Done(a) => effects(norm(h.ret(a)))
    case o: NormC.Op[p, j, o2, a, f, x] => o.in match
      case _: In.Here[f2, t] => h match
        case h: Answering[E, T, A, B] => handledC(h, o.k(h.value(o.op)))
        case h: Translating[E, T, A, B] => bindC(effects(norm(h(o.op))), (x: x) => handledC(h, o.k(x)))
        case h: Linear[E, T, A, B] => normC(h[J, O, x](o.op, (x: x) => reifyC(handledC(h, o.k(x)))))
      case th: In.There[f2, g, t] => NormC.Op(o.op, th.in, (x: x) => handledC(h, o.k(x)))
      case at: In.At[f2, r2] => h.at(at) match
        case null => NormC.Op(o.op, at.again, (x: x) => handledC(h, o.k(x)))
        case s: In.Same[E, f] => s match
          case In.Same.Refl() => handledC(h, NormC.Op(o.op, In.Here[f, T](), o.k))
    case s: NormC.Shifted[p, t2, i1, o1, x1, y, j, o2, a] => s.at match
      case Nearest.There(at1) => NormC.Shifted(at1, s.f, (y: y) => handledC(h, s.k(y)))
      case _: Nearest.Here[t3] => Nearest.absurd(s.at)

  def stated[E[_], T <: Row, S, A, B](h: Stateful[E, T, S, A, B], s: S, n: Norm[E +: T, A]): Norm[T, B] = n match
    case Norm.Done(a) => norm(h.ret(s, a))
    case o: Norm.Op[r, a, f, x] => o.in match
      case _: In.Here[f2, t] => stated(h, h.next(s, o.op), o.k(h.value(s, o.op)))
      case th: In.There[f2, g, t] => Norm.Op(o.op, th.in, (x: x) => stated(h, s, o.k(x)))
      case at: In.At[f2, r2] => h.at(at) match
        case null => Norm.Op(o.op, at.again, (x: x) => stated(h, s, o.k(x)))
        case sm: In.Same[E, f] => sm match
          case In.Same.Refl() => stated(h, s, Norm.Op(o.op, In.Here[f, T](), o.k))

  /** a state under a `$`: taken into a shift0's rest as it is — each resumption from it */
  def statedC[E[_], T <: Row, S, J, O, A, B](h: Stateful[E, T, S, A, B], s: S, n: NormC[E +: T, J, O, A]): NormC[T, J, O, B] =
    n match
      case NormC.Done(a) => effects(norm(h.ret(s, a)))
      case o: NormC.Op[p, j, o2, a, f, x] => o.in match
        case _: In.Here[f2, t] => statedC(h, h.next(s, o.op), o.k(h.value(s, o.op)))
        case th: In.There[f2, g, t] => NormC.Op(o.op, th.in, (x: x) => statedC(h, s, o.k(x)))
        case at: In.At[f2, r2] => h.at(at) match
          case null => NormC.Op(o.op, at.again, (x: x) => statedC(h, s, o.k(x)))
          case sm: In.Same[E, f] => sm match
            case In.Same.Refl() => statedC(h, s, NormC.Op(o.op, In.Here[f, T](), o.k))
      case sh: NormC.Shifted[p, t2, i1, o1, x1, y, j, o2, a] => sh.at match
        case Nearest.There(at1) => NormC.Shifted(at1, sh.f, (y: y) => statedC(h, s, sh.k(y)))
        case _: Nearest.Here[t3] => Nearest.absurd(sh.at)

  def viewed[E[_], R <: Row, T <: Row, A](rm: Removed[E, R, T], n: Norm[R, A]): Norm[E +: T, A] = n match
    case Norm.Done(a) => Norm.Done(a)
    case o: Norm.Op[r, a, f, x] => Norm.Op(o.op, Removed.relocate(o.in, rm), (x: x) => viewed(rm, o.k(x)))

  def viewedC[E[_], R <: Row, T <: Row, J, O, A](rm: Removed[E, R, T], n: NormC[R, J, O, A]): NormC[E +: T, J, O, A] = n match
    case NormC.Done(a) => NormC.Done(a)
    case o: NormC.Op[p, j, o2, a, f, x] => NormC.Op(o.op, Removed.relocate(o.in, rm), (x: x) => viewedC(rm, o.k(x)))
    case s: NormC.Shifted[p, t2, i1, o1, x1, y, j, o2, a] =>
      NormC.Shifted(Nearest.relocate(s.at, rm), s.f, (y: y) => viewedC(rm, s.k(y)))

  /** a `$`: its body's value through `ret`, its answer a program outside; a shift0 of its own given its rest with the
   * `$` put back, as a program outside; the operations of the row outside past it */
  def dollarC[T <: Row, I, I2, O2, B, A](n: NormC[Delim[T], I, Cont[T, I2, O2, B], A], ret: A => I): NormC[T, I2, O2, B] =
    n match
      case NormC.Done(a) => normC(ret(a))
      case o: NormC.Op[p, i, p2, a, e, x] => o.in match
        case In.There(in) => NormC.Op(o.op, in, (x: x) => dollarC(o.k(x), ret))
        case In.Here() => ControlOp.absurd(o.op)
        case at: In.At[e2, r2] => NormC.Op(o.op, at.again, (x: x) => dollarC(o.k(x), ret))
      case s: NormC.Shifted[p, t2, i1, o1, x1, y, i, p2, a] => s.at match
        case _: Nearest.Here[t3] => normC(s.f((y: y) => reifyC(dollarC(s.k(y), ret))))
        case Nearest.There(_) => Nearest.absurd(s.at)

  /** a `reset`: a `$` whose answer is a value; a shift0's rest with it put back a program outside, its value run */
  def lowerC[R <: Row, S, O](n: NormC[Delim[R], S, O, S]): NormC[R, Nothing, Nothing, O] = n match
    case NormC.Done(a) => NormC.Done(a)
    case o: NormC.Op[p, s, o2, a, e, x] => o.in match
      case In.There(in) => NormC.Op(o.op, in, (x: x) => lowerC(o.k(x)))
      case In.Here() => ControlOp.absurd(o.op)
      case at: In.At[e2, r2] => NormC.Op(o.op, at.again, (x: x) => lowerC(o.k(x)))
    case sh: NormC.Shifted[p, t2, i1, o1, x1, y, s, o2, a] => sh.at match
      case _: Nearest.Here[t3] =>
        NormC.Done(sh.f((y: y) => Bind(Lift[R, o1, Cont[R, i1, o1, x1]](reifyC(lowerC(sh.k(y)))),
          (q: Cont[R, i1, o1, x1]) => q)))
      case Nearest.There(_) => Nearest.absurd(sh.at)

  /** a program with no effect left: its value */
  def value[A](n: Norm[Pure, A]): A = n match
    case Norm.Done(a) => a
    case o: Norm.Op[r, a, e, x] => In.absurd(o.in)

  // THE PROGRAMS

  /** each `Get` answered from the state plus `n`, `k` once or twice: made of `St`, an operation of it in its clause */
  def askSt[T <: Row](n: Int, twice: Boolean)(using In[St, T]) = new Deep[Ask, T, Int, Int]:
    def ret(a: Int) = pure(a * 2)
    def apply[X](op: Ask[X], k: X => Int ! T) = op match
      case Ask.Get =>
        val r = rows[T]
        r.perform(St.Get).flatMap(s => if twice then k(s + n).flatMap(a => k(a % 7).map(_ + a)) else k(s + n))

  /** each `Get` translated to a program over `St`: the state plus `n`, the state moved on */
  def askFromSt[T <: Row](n: Int)(using In[St, T]) = new Translate[Ask, T, Int]:
    def apply[X](op: Ask[X]) = op match
      case Ask.Get =>
        val r = rows[T]
        r.perform(St.Get).flatMap(s => r.perform(St.Put(s + 1)).map(_ => s + n))

  /** a random program over a row with the four effects: operations, binds, maps, defers, and handlers of each kind in
   * it, at the head of the row and past it. Every random choice made here, none in the closures */
  def gen[R <: Row](r: Random, depth: Int)(using In[Ask, R], In[St, R], In[Choose, R], In[Throws, R]): Int ! R =
    val c = r.nextInt(5)
    if depth == 0 then pure(c)
    else r.nextInt(21) match
      case 0 => pure(c)
      case 1 =>
        val rest = gen[R](r, depth - 1)
        perform[Ask, R, Int](Ask.Get).flatMap(n => rest.map(_ + n))
      case 2 =>
        val rest = gen[R](r, depth - 1)
        perform[St, R, Int](St.Get).flatMap(s => rest.map(_ * 2 + s))
      case 3 =>
        val rest = gen[R](r, depth - 1)
        perform[St, R, Unit](St.Put(c)).flatMap(_ => rest)
      case 4 =>
        val (l, rr) = (gen[R](r, depth - 1), gen[R](r, depth - 1))
        perform[Choose, R, Boolean](Choose.Flip).flatMap(b => if b then l else rr.map(_ + 1))
      case 5 =>
        val rest = gen[R](r, depth - 1)
        if c % 2 == 0 then perform[Throws, R, Nothing](Throws.Raise("e" * c)).flatMap(_ => rest) else rest
      case 6 =>
        val (l, rr) = (gen[R](r, depth - 1), gen[R](r, depth - 1))
        l.flatMap(x => if x % 2 == 0 then rr.map(_ + x) else pure(x - c))
      case 7 =>
        val inner = gen[R](r, depth - 1)
        Cont.defer(inner).map(_ + c)
      case 8 => gen[Ask +: R](r, depth - 1).handle(reader(c))
      case 9 => gen[St +: R](r, depth - 1).handle(state(c)).map((s, a) => s + a)
      case 10 => gen[Choose +: R](r, depth - 1).handle(choose).map(_.sum)
      case 11 => gen[Throws +: R](r, depth - 1).handle(attempt).map(_.fold(_.length, identity))
      case 12 => gen[Ask +: R](r, depth - 1).handle(askSt[R](c, r.nextBoolean()))
      /** past the head */
      case 13 => gen[Ask +: St +: R](r, depth - 1).handle(state(c)).map(_._2).handle(reader(c + 1))
      case 14 => gen[St +: Choose +: R](r, depth - 1).handle(choose).map(_.sum).handle(state(c)).map(_._2)
      case 16 => gen[Ask +: R](r, depth - 1).handle(askFromSt[R](c))
      case 17 => gen[Choose +: Ask +: R](r, depth - 1).handle(askFromSt[Choose +: R](c)).handle(choose).map(_.sum)
      /** a shift0 up to a reset of control as an effect, k dropped, once, twice */
      case 18 =>
        val rest = gen[R](r, depth - 1)
        val how = r.nextInt(3)
        Shift.shift0[Int, R, Int, Shifts[Int, R] +: R](k => how match
          case 0 => pure(c)
          case 1 => k(c)
          case _ => k(c).flatMap(a => k(a + 1).map(_ + a)))
          .flatMap(x => widen[Shifts[Int, R], R, Int](rest).map(_ + x)).handle(Shift.reset[Int, R])
      /** a shift, its body under the reset — Danvy–Filinski's */
      case 19 =>
        val rest = gen[R](r, depth - 1)
        Shift.shift[Int, R, Int, Shifts[Int, R] +: R](k => k(c).flatMap(a => k(a * 2)))
          .flatMap(x => widen[Shifts[Int, R], R, Int](rest).map(_ - x)).handle(Shift.reset[Int, R])
      /** k kept in the answer, resumed after the handler has answered, under the handlers outside it */
      case 20 => gen[Ask +: R](r, depth - 1).handle(keep[R]).flatMap(kept => drained[R](c, kept))
      case _ => gen[Ask +: Throws +: R](r, depth - 1).handle(attempt).map(_.fold(_.length, identity))
        .handle(askSt[R](c, true))

  type R4 = Ask +: St +: Choose +: Throws +: Pure

  /** each `Get` given back with its rest, k kept: the rest resumed later, outside the handler */
  def keep[R <: Row] = new Deep[Ask, R, Int, KeptR[R]]:
    def ret(a: Int) = pure(KeptR.Value(a))
    def apply[X](op: Ask[X], k: X => KeptR[R] ! R) = op match { case Ask.Get => pure(KeptR.Rest(k)) }

  /** the rests kept, resumed one by one with `c` and a state moved on before each */
  def drained[R <: Row](c: Int, kept: KeptR[R])(using In[St, R]): Int ! R = kept match
    case KeptR.Value(v) => pure(v)
    case KeptR.Rest(k) => perform[St, R, Unit](St.Put(c)).flatMap(_ => k(c)).flatMap(next => drained(c + 1, next))

  /** a program stepped, each `Get` answered `n` by the driver */
  def driven[A](s: Step[Ask +: Pure, A], n: Int): A =
    @scala.annotation.tailrec def go(s: Step[Ask +: Pure, A]): A = s match
      case Step.Done(a) => a
      case Step.Failed(e) => throw e
      case Step.Cancelled() => throw RuntimeException("cancelled")
      case y: Step.Yield[r, a, e, x] => y.in match
        case _: In.Here[f, t] => go(y.resume(asked(y.op, n)))
        case th: In.There[f, g, t] => In.absurd(th.in)
        case at: In.At[f, r] => In.unhandled(at)
    go(s)
  def asked[X](op: Ask[X], n: Int): X = op match { case Ask.Get => n }

  test("step: random programs stepped, their Gets answered by the driver — as the meaning with a reader") {
    val r = Random(37)
    for i <- 1 to 300 do
      val p = gen[R4](r, 4).handle(attempt).handle(choose).handle(state(1))
      assertEquals(driven(p.step, 2), value(norm(p.handle(reader(2)))), s"program $i")
  }

  test("the semantics pinned: the answers of random programs, their digest — changed only knowingly") {
    val r = Random(29)
    val answers = (1 to 1000).map(_ => gen[R4](r, 4).handle(attempt).handle(choose).handle(state(1)).handle(reader(2)).run)
    assertEquals(answers.toString.hashCode, Digest)
  }

  test("effects: random programs, handlers of each kind in them and around them — the machine as the meaning") {
    val r = Random(17)
    for i <- 1 to 500 do
      val p = gen[R4](r, 4).handle(attempt).handle(choose).handle(state(1)).handle(reader(2))
      assertEquals(p.run, value(norm(p)), s"program $i")
  }

  test("effects: random programs, the choice outermost — each path its own state") {
    val r = Random(19)
    for i <- 1 to 500 do
      val p = gen[R4](r, 4).handle(reader(3)).handle(attempt).handle(state(0)).handle(choose)
      assertEquals(p.run, value(norm(p)), s"program $i")
  }

  /** a random program of control over `R`: values, binds, maps, defers, shift0s calling `k` none, once, twice, `$`s
   * of their own; programs of effects in it, closed but for `Ask` and `Choose` — a choice's `k` taken past the `$`s
   * and the `reset`s; a `reset` in it, a program of control again within */
  /** the rest of a shift0 under the `$` over `R`: a program of effects over `R` */
  type L[R <: Row] = Int ! R
  /** a program under the `$` over `R`, its answers `L[R]` */
  type B[R <: Row] = Cont[Delim[R], L[R], L[R], Int]

  def body[R <: Row](r: Random, depth: Int)(using In[Ask, R], In[Choose, R]): B[R] =
    val c = r.nextInt(5)
    def ret(n: Int): B[R] = Return(n)
    def shift0(f: (Int => L[R]) => L[R]): B[R] = Shift0[Delim[R], R, Nothing, Nothing, Int, L[R], Int](Nearest.Here(), f)
    /** the `$` over `R`: a program of effects over `R` */
    def dollar(b: B[R]): L[R] = Dollar[R, L[R], Nothing, Nothing, Int, Int](b, a => Return(a))
    if depth == 0 then ret(c)
    else r.nextInt(15) match
      case 0 => ret(c)
      case 1 => body[R](r, depth - 1).map(_ * 3 + c)
      /** handlers under the `$`, of each kind leaving the rest — a shift0 under them takes them into its rest */
      case 11 => under[R](r, depth - 1).handle(reader(c)).handle(state(c)).map(_._2)
      case 12 => under[R](r, depth - 1).handle(askFromSt[St +: Delim[R]](c)).handle(state(c)).map(_._2)
      case 13 => under[R](r, depth - 1).handle(once[St +: Delim[R]](c)).handle(state(c)).map(_._2)
      /** the state taken from past the head: a view under the `$` */
      case 14 => under[R](r, depth - 1).handle(state(c)).map(_._2).handle(reader(c + 1))
      // not here: a handler with a state of its own (a cell): when its value is asked is the machine's, not the meaning's
      case 2 =>
        val (l, rr) = (body[R](r, depth - 1), body[R](r, depth - 1))
        l.flatMap(x => if x % 2 == 0 then rr.map(_ + x) else ret(x - c))
      case 3 =>
        val inner = body[R](r, depth - 1)
        r.nextInt(5) match
          case 0 => shift0(_ => Return(c)).flatMap(_ => inner)
          case 1 => shift0(k => k(c)).flatMap(x => inner.map(_ + x))
          case 2 => shift0(k => k(c).flatMap(k)).flatMap(x => inner.map(_ * x))
          case 3 => shift0(k => k(c).flatMap(a => k(a + 1).map(_ + a))).flatMap(x => inner.map(_ - x))
          case _ => shift0(k => dollar(inner).flatMap(k)).map(_ + c)
      /** a `$` inside, its body a row deeper, a shift0 of its own up to it: its `k` a program under the outer `$` */
      case 4 =>
        val inner: Cont[Delim[Delim[R]], B[R], B[R], Int] = r.nextInt(3) match
          case 0 => Return(c)
          case 1 => Shift0[Delim[Delim[R]], Delim[R], L[R], L[R], Int, B[R], Int](Nearest.Here(), k => k(c).flatMap(k)).map(_ + 1)
          case _ => Shift0[Delim[Delim[R]], Delim[R], L[R], L[R], Int, B[R], Int](Nearest.Here(), k => k(c).flatMap(a => k(a * 2)))
        val after = body[R](r, depth - 1)
        Dollar[Delim[R], B[R], L[R], L[R], Int, Int](inner, a => Return(a + c)).flatMap(x => after.map(_ + x))
      case 5 =>
        val inner = body[R](r, depth - 1)
        Defer(() => inner).map(_ + c)
      /** a program of effects under the `$`, its own effects handled in it, `Ask` and `Choose` out past the `$`s */
      case 6 =>
        val p = gen[St +: Throws +: Delim[R]](r, depth - 1).handle(attempt).map(_.fold(_.length, identity))
          .handle(state(c)).map(_._2)
        val after = body[R](r, depth - 1)
        p.cont[L[R]].flatMap(x => after.map(_ + x))
      case 7 =>
        val after = body[R](r, depth - 1)
        perform[Ask, Delim[R], Int](Ask.Get).cont[L[R]].flatMap(n => after.map(_ * n + c))
      case 8 =>
        val (l, rr) = (body[R](r, depth - 1), body[R](r, depth - 1))
        perform[Choose, Delim[R], Boolean](Choose.Flip).cont[L[R]].flatMap(b => if b then l else rr.map(_ + c))
      /** a `reset` inside — a `$` a row deeper, a program of effects under the outer `$` */
      case 9 =>
        val inner = body[Delim[R]](r, depth - 1)
        val after = body[R](r, depth - 1)
        Dollar[Delim[R], L[Delim[R]], Nothing, Nothing, Int, Int](inner, a => Return(a)).cont[L[R]]
          .flatMap(x => after.map(_ + x))
      case _ =>
        val (l, rr) = (body[R](r, depth - 1), body[R](r, depth - 1))
        l.flatMap(x => rr.map(_ + x))

  /** each `Get` answered `n`, `k` once, the value plus one: a `Linear` */
  def once[T <: Row](n: Int) = new Linear[Ask, T, Int, Int]:
    def ret(a: Int) = pure(a)
    def apply[I, O, X](op: Ask[X], k: X => Cont[T, I, O, Int]) = op match
      case Ask.Get => k(n).map(_ + 1)

  /** a random program under the `$` over `R`, under handlers of `Ask` and `St`: operations of theirs, of `Choose`
   * past the `$`, shift0s past the handlers, `$`s of its own. Every random choice made here, none in the closures */
  type Q[R <: Row] = Ask +: St +: Delim[R]
  type BQ[R <: Row] = Cont[Q[R], L[R], L[R], Int]

  def under[R <: Row](r: Random, depth: Int)(using In[Ask, R], In[Choose, R]): BQ[R] =
    val c = r.nextInt(5)
    def ret(n: Int): BQ[R] = Return(n)
    if depth == 0 then ret(c)
    else r.nextInt(10) match
      case 0 => ret(c)
      case 1 =>
        val rest = under[R](r, depth - 1)
        perform[Ask, Q[R], Int](Ask.Get).flatMap(n => rest.map(_ + n))
      case 2 =>
        val rest = under[R](r, depth - 1)
        perform[St, Q[R], Int](St.Get).flatMap(n => rest.map(_ * 2 + n))
      case 3 =>
        val rest = under[R](r, depth - 1)
        perform[St, Q[R], Unit](St.Put(c)).flatMap(_ => rest)
      case 4 =>
        val (l, rr) = (under[R](r, depth - 1), under[R](r, depth - 1))
        perform[Choose, Q[R], Boolean](Choose.Flip).flatMap(b => if b then l else rr.map(_ + c))
      case 5 =>
        val inner = under[R](r, depth - 1)
        r.nextInt(4) match
          case 0 => rows[Q[R]].shift0[Int, Int](_ => Return(c)).flatMap(_ => inner)
          case 1 => rows[Q[R]].shift0[Int, Int](k => k(c)).flatMap(x => inner.map(_ + x))
          case 2 => rows[Q[R]].shift0[Int, Int](k => k(c).flatMap(k)).flatMap(x => inner.map(_ * x))
          case _ => rows[Q[R]].shift0[Int, Int](k => k(c).flatMap(a => k(a + 1).map(_ + a))).flatMap(x => inner.map(_ - x))
      /** a `$` of its own under the handlers, a shift0 up to it */
      case 6 =>
        val inner: Cont[Delim[Q[R]], BQ[R], BQ[R], Int] = r.nextInt(3) match
          case 0 => Return(c)
          case 1 => Shift0[Delim[Q[R]], Q[R], L[R], L[R], Int, BQ[R], Int](Nearest.Here(), k => k(c).flatMap(k)).map(_ + 1)
          case _ =>
            perform[St, Delim[Q[R]], Int](St.Get).flatMap(n =>
              Shift0[Delim[Q[R]], Q[R], L[R], L[R], Int, BQ[R], Int](Nearest.Here(), k => k(c + n).flatMap(a => k(a * 2))))
        val after = under[R](r, depth - 1)
        Dollar[Q[R], BQ[R], L[R], L[R], Int, Int](inner, a => Return(a + c)).flatMap(x => after.map(_ + x))
      case 7 =>
        val inner = under[R](r, depth - 1)
        Defer(() => inner).map(_ + c)
      case 8 =>
        val (l, rr) = (under[R](r, depth - 1), under[R](r, depth - 1))
        l.flatMap(x => if x % 2 == 0 then rr.map(_ + x) else ret(x - c))
      case _ =>
        val (l, rr) = (under[R](r, depth - 1), under[R](r, depth - 1))
        l.flatMap(x => rr.map(_ + x))

  test("control: random programs of shift0 and $, effects out of them past the $s, handlers under the $s — the machine as the meaning") {
    val r = Random(23)
    type R = Choose +: Ask +: Pure
    for i <- 1 to 1500 do
      val p = Dollar[R, L[R], Nothing, Nothing, Int, Int](body[R](r, 4), a => Return(a)).handle(choose).handle(reader(5))
      assertEquals(p.run, value(norm(p)), s"program $i")
  }

  test("control alone: a program of control with no effect, run, as its meaning") {
    val r = Random(31)
    for i <- 1 to 200 do
      val c: L[Pure] = Dollar[Pure, L[Pure], Nothing, Nothing, Int, Int](body0(r, 4), a => Return(a))
      assertEquals(c.run, value(norm(c)), s"program $i")
  }

  /** with no effect: shift0s and `$`s only */
  def body0(r: Random, depth: Int): B[Pure] =
    val c = r.nextInt(5)
    if depth == 0 then Return(c)
    else r.nextInt(4) match
      case 0 => Return(c)
      case 1 =>
        val inner = body0(r, depth - 1)
        Shift0[Reset, Pure, Nothing, Nothing, Int, L[Pure], Int](Nearest.Here(), k => k(c).flatMap(a => k(a + 1).map(_ + a)))
          .flatMap(x => inner.map(_ - x))
      case 2 =>
        val (l, rr) = (body0(r, depth - 1), body0(r, depth - 1))
        l.flatMap(x => rr.map(_ + x))
      case _ =>
        val inner: Cont[Delim[Reset], B[Pure], B[Pure], Int] =
          Shift0[Delim[Reset], Reset, L[Pure], L[Pure], Int, B[Pure], Int](Nearest.Here(), k => k(c).flatMap(a => k(a * 2)))
        val after = body0(r, depth - 1)
        Dollar[Reset, B[Pure], L[Pure], L[Pure], Int, Int](inner, a => Return(a + c)).flatMap(x => after.map(_ + x))

/** an answer holding a kept rest */
enum KeptR[R <: Row]:
  case Value(v: Int)
  case Rest(k: Int => KeptR[R] ! R)
