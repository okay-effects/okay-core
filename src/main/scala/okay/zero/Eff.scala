package okay.zero

import scala.annotation.tailrec
import Cont0.*

/** WHAT A DELIMITER'S BODY ANSWERS IT, of the answer `V`: its answer; an operation of its row — at its path — with the
 * rest, `k`; or a capture up to it, a `shift`, with its body and the rest. A program of the delimiter's own level each
 * rest. Made and read in this package only */
sealed trait Res[R <: Row, J, V]
object Res:
  private[zero] final case class Done[R <: Row, J, V](v: V) extends Res[R, J, V]
  private[zero] final case class Req[R <: Row, J, V, E[_], X](op: E[X], in: In[E, R], k: X => Cont0[J, J, Res[R, J, V]])
    extends Res[R, J, V]
  private[zero] final case class Ctl[R <: Row, J, V, A, I](f: (A => Cont0[Cont0[J, J, Res[R, J, V]], Cont0[J, J, Res[R, J, V]], I]) => Cont0[Cont0[J, J, Res[R, J, V]], Cont0[J, J, Res[R, J, V]], V],
                                                           k: A => Cont0[J, J, Res[R, J, I]]) extends Res[R, J, V]

/** A LEVEL: the answer at a point of a body over `R`, its delimiter's outside at `J` — a program there, pure in its
 * answer, yielding the delimiter the body's answer `V`, or a request */
opaque type Level[R <: Row, J, V] = Cont0[J, J, Res[R, J, V]]

/** A PROGRAM over the row `R`, its nearest delimiter's outside at `J`: of value `A`, which, with a continuation
 * whose answer is `I`, answers `O`. The one type of both modules — Control's shift and reset, Effects' operations
 * and handlers — over the kernel, and no bridge to it: no raw `shift0` or `$` reaches a request */
opaque type Eff[R <: Row, J, I, O, A] = Cont0[Level[R, J, I], Level[R, J, O], A]

/** THE CONTEXT a program is written in, as master's: its row `R`, its nearest delimiter's outside `J`, the answer at
 * this point `T` — what an operation, a handler, a reset and a shift take their types from, by its members. The
 * nearest wins: a delimiter gives its body one of its own. A type, nothing at run time; made here only */
abstract class Ctx private[zero] ():
  type R <: Row
  type J
  type T
  /** a program here, pure in the answer */
  type P[A] = Eff[R, J, T, T, A]
  /** the level a delimiter here gives its body */
  type Inner = Level[R, J, T]
  /** the context under a handler of `E` here, at its answer `Ans` */
  type Handled[E[_], Ans] = At[E +: R, Inner, Ans]
  /** a handler's body here: in its context, of value `A`, at its answer `Ans` */
  type Body[E[_], Ans, A] = Handled[E, Ans] ?=> Eff[E +: R, Inner, Ans, Ans, A]
  /** the context in a reset here, its body written at `S` */
  type Reset[S] = InReset[R, J, T, S]
  /** this context at the answer `O`: a shift's body */
  type Answer[O] = At[R, J, O]

/** contexts made in this package: a library's own runner (`Async.runAsync`) re-entering at the top */
private[zero] object Ctxs:
  def at[R0 <: Row, J0, T0]: At[R0, J0, T0] = At()

/** a context at `R0`, `J0`, `T0` */
class At[R0 <: Row, J0, T0] private[zero] () extends Ctx:
  type R = R0
  type J = J0
  type T = T0

extension [R <: Row, J, I, O, A](p: Eff[R, J, I, O, A])
  def flatMap[I2, B](f: A => Eff[R, J, I2, I, B]): Eff[R, J, I2, O, B] = p.flatMap(f)
  def map[B](f: A => B): Eff[R, J, I, O, B] = p.map(f)

def pure[R <: Row, J, T, A](a: A): Eff[R, J, T, T, A] = Return(a)

/** `delay(p)`: the program `p`, built when the loop gets to it — a tail call */
def delay[R <: Row, J, I, O, A](p: => Eff[R, J, I, O, A]): Eff[R, J, I, O, A] = Delay(() => p)

/** `run(body)`: the body at the top, under a delimiter of its own — the top's: nothing to ask over `Empty`, a `shift`
 * up to it the body's — its answer its value */
def run[A](body: At[Empty, A, A] ?=> Eff[Empty, A, A, A, A]): A = top(Dollar(body(using At()), (a: A) => Return(Res.Done[Empty, A, A](a)))).value

private def top[A](out: Cont0[A, A, Res[Empty, A, A]]): Cont0[A, A, A] =
  Bind(out, (res: Res[Empty, A, A]) => res match
    case Res.Done(a) => Return[A, A](a)
    case c: Res.Ctl[r, j, v, a, i] => top(again(c)))

// THE NODES Control and Effects are written with: an operation, a `shift`, a handler, a reset, a raw capture

/** an operation of `E` at its path, at any answer */
private[zero] def request[E[_], R <: Row, J, T, X](op: E[X], in: In[E, R]): Eff[R, J, T, T, X] = Op[R, J, T, E, X](op, in)

/** a `shift` to the nearest delimiter */
private[zero] def control[R <: Row, J, A, I, O](f: (A => Eff[R, J, O, O, I]) => Eff[R, J, O, O, O]): Eff[R, J, I, O, A] = Ctl(f)

/** a handler of `E`: its body, its answers read by `h` */
private[zero] def handleNode[E[_], R <: Row, J, T, A, Ans](h: Handler[[X] =>> Eff[R, J, T, T, X], E, A, Ans],
    body: Eff[E +: R, Level[R, J, T], Ans, Ans, A]): Eff[R, J, T, T, Ans] = Handle(body, h)

/** a reset: its body, every operation passed on */
private[zero] def resetNode[R <: Row, J, T, S, O](body: Eff[R, Level[R, J, T], S, O, S]): Eff[R, J, T, T, O] = Reset(body)

/** a capture up to the nearest delimiter, its outside over `R` at `T`: `k` the rest under it again, yielding its
 * answers; the body in the delimiter's place — shift0's */
private[zero] def capture[R <: Row, J, T, A, I, O](f: (A => Eff[R, J, T, T, Res[R, Level[R, J, T], I]]) => Eff[R, J, T, T, Res[R, Level[R, J, T], O]])
    : Eff[R, Level[R, J, T], I, O, A] =
  Shift0[Level[R, J, T], Level[R, J, T], Res[R, Level[R, J, T], I], Level[R, Level[R, J, T], O], A](f)

// THE MACHINE: a handler's and a reset's body run in a loop of their own, to where it leaves the delimiter — its
// answer by `ret`; an operation answered in place, its handler's by the handlers in force; else at the delimiter, its
// own by the clause, another's passed on outside; a resumption, a handler, a reset entered. What it does not take —
// a `shift`, a raw capture — goes to the delimiters' reading below, the delimiter's answers read as values

/** a delimiter the machine runs: a handler of `E` (its body over `E +: R`, the outside over `R` at `T`, at the answer
 * `Ans` — `ret` the handler's, or none for a shift's body), or a reset (its body over `R`, written at `S`) */
private[zero] enum Delim[RB <: Row, L, I0, V, A]:
  case Hand[E[_], R <: Row, J, T, A, Ans](h: Handler[[X] =>> Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], X], E, ?, Ans], ret: A => Ans)
    extends Delim[E +: R, Cont0[J, J, Res[R, J, T]], Ans, Ans, A]
  case Rst[R <: Row, J, T, S, V]() extends Delim[R, Cont0[J, J, Res[R, J, T]], S, V, S]

/** THE HANDLERS IN FORCE around a loop as it runs, innermost first, one per effect of its row — or not known, past a
 * delimiter the machine did not enter: an operation then goes the way it always may, to the delimiters */
private[zero] enum Ev[R <: Row]:
  case Unknown[R <: Row]() extends Ev[R]
  case Entry[E[_], T <: Row](h: Handler[?, E, ?, ?], outer: Ev[T]) extends Ev[E +: T]

/** the handler at the path, if it answers in place — `shift0 k. k(v)` is `v` */
@tailrec private def inPlace[E[_], R <: Row](in: In[E, R], ev: Ev[R]): Answering[?, E, ?, ?] | Null = ev match
  case e: Ev.Entry[e, t] => in match
    case In.Here() => e.h match
      case a: Answering[?, E, ?, ?] => a
      case _ => null
    case In.There(out) => inPlace(out, e.outer)
  case _ => null

/** the loop: `c` with the stack `k` under the delimiter `d`, the handlers in force inside it `ev` — to the program
 * outside the delimiter */
@tailrec private def deliver[RB <: Row, L, I0, V, A, T, X](c: Cont0[T, Cont0[L, L, Res[RB, L, V]], X],
    k: Frames[X, Cont0[L, L, Res[RB, L, I0]], T, A], d: Delim[RB, L, I0, V, A], ev: Ev[RB]): Cont0[L, L, V] = c match
  case b: Bind[i, t, o, a, bb] => b.m match
    case r: Return[s, aa] => deliver[RB, L, I0, V, A, T, X](b.f(r.a), k, d, ev)
    case _ => deliver[RB, L, I0, V, A, t, a](b.m, Frames.Then(b.f, k), d, ev)
  case r: Return[s, x] => k match
    case Frames.End() => Return[L, V](ret(d, r.a))
    case th: Frames.Then[a1, x1, i1, t1, t2, b1] => deliver[RB, L, I0, V, A, t1, x1](th.f(r.a), th.next, d, ev)
    case mp: Frames.Mapped[a1, x1, i1, t1, b1] => deliver[RB, L, I0, V, A, T, x1](Return(mp.f(r.a)), mp.next, d, ev)
  // the handler at the operation's path answering in place, its value at once; not known so, to the delimiter
  case o: OpK[r, j, t, e, x, i, b] => inPlace[e, RB](o.in, ev) match
    case null => k match
      case Frames.End() => route[RB, L, I0, V, A, e, x](o.op, o.in, o.k, d)
      case _ => route[RB, L, I0, V, A, e, x](o.op, o.in, Frames.Then(o.k, k), d)
    case v => deliver[RB, L, I0, V, A, T, X](o.k(v.value(o.op)), k, d, ev)
  case o: Op[r, j, t, e, x] => inPlace[e, RB](o.in, ev) match
    case null => route[RB, L, I0, V, A, e, x](o.op, o.in, k, d)
    case v => deliver[RB, L, I0, V, A, T, X](Return(v.value(o.op)), k, d, ev)
  case m: Map[i, o, a, b] => deliver[RB, L, I0, V, A, T, a](m.m, Frames.Mapped(m.f, k), d, ev)
  case dl: Delay[i, o, a] => deliver[RB, L, I0, V, A, T, X](dl.c(), k, d, ev)
  case r: Resume[rb, l, i0, v, a, x] => deliver[RB, L, I0, V, A, T, X](enter[rb, l, i0, v, a, x](r, inner[rb, RB, L, V, i0, v, a](r.d, ev)), k, d, ev)
  case h: Handle[e, r, j, t, a, ans] => deliver[RB, L, I0, V, A, T, X](enterHandle[e, r, j, t, a, ans](h, ev), k, d, ev)
  case s: Reset[r, j, t, sv, o] => deliver[RB, L, I0, V, A, T, X](enterReset[r, j, t, sv, o](s, ev), k, d, ev)
  case _ => bail(c, k, d)

/** the body's value to the delimiter's answer */
private def ret[RB <: Row, L, I0, V, A](d: Delim[RB, L, I0, V, A], a: A): I0 = d match
  case h: Delim.Hand[e, r, j, t, a, ans] => h.ret(a)
  case _: Delim.Rst[r, j, t, s, v] => a

/** an operation not answered in place, at the delimiter: a handler's own — its clause, `k` the rest under it again;
 * another's — on outside, one handler shorter, or as it is past a reset */
private def route[RB <: Row, L, I0, V, A, E[_], X](op: E[X], in: In[E, RB], k: X => Cont0[Cont0[L, L, Res[RB, L, I0]], Cont0[L, L, Res[RB, L, V]], A],
    d: Delim[RB, L, I0, V, A]): Cont0[L, L, V] = d match
  case h: Delim.Hand[e, r, j, t, a, ans] => in match
    case In.Here() => h.h(op, Resumed(k, d))
    case In.There(o) => OpK[r, j, t, E, X, Cont0[j, j, Res[r, j, t]], ans](op, o, Resumed(k, d))
  case _: Delim.Rst[r, j, t, s, v] => OpK[r, j, t, E, X, Cont0[j, j, Res[r, j, t]], v](op, in, Resumed(k, d))

/** a captured rest as `k`: applied, a `Resume` — lazily, so a clause that keeps it runs nothing by making it */
private final class Resumed[RB <: Row, L, I0, V, A, X](k: X => Cont0[Cont0[L, L, Res[RB, L, I0]], Cont0[L, L, Res[RB, L, V]], A], d: Delim[RB, L, I0, V, A])
    extends (X => Cont0[L, L, V]):
  def apply(x: X): Cont0[L, L, V] = Resume(x, k, d)

/** the handlers in force inside the delimiter `d`, those outside it `ev`: a handler's in front, a reset's the same */
private def inner[RB <: Row, RO <: Row, LO, VO, I0, V, A](d: Delim[RB, Cont0[LO, LO, Res[RO, LO, VO]], I0, V, A], ev: Ev[RO]): Ev[RB] = d match
  case h: Delim.Hand[e, r, j, t, a, ans] => Ev.Entry(h.h, ev)
  case _: Delim.Rst[r, j, t, s, v] => ev

/** a resumption: its rest under its delimiter again, in a loop of its own */
private[zero] def enter[RB <: Row, L, I0, V, A, X](r: Resume[RB, L, I0, V, A, X], ev: Ev[RB]): Cont0[L, L, V] =
  deliver(r.k(r.x), Frames.End(), r.d, ev)

/** a handler entered: its body in a loop of its own, the handler in front of those in force */
private[zero] def enterHandle[E[_], R <: Row, J, T, A, Ans](h: Handle[E, R, J, T, A, Ans], ev: Ev[R]): Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Ans] =
  deliver(h.body, Frames.End(), Delim.Hand[E, R, J, T, A, Ans](h.h, h.h.ret), Ev.Entry(h.h, ev))

/** a reset entered: its body in a loop of its own */
private[zero] def enterReset[R <: Row, J, T, S, O](s: Reset[R, J, T, S, O], ev: Ev[R]): Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], O] =
  deliver(s.body, Frames.End(), Delim.Rst[R, J, T, S, O](), ev)

/** a resumption entered with nothing known of the handlers around it — by the kernel's loop */
private[zero] def enterUnknown[RB <: Row, L, I0, V, A, X](r: Resume[RB, L, I0, V, A, X]): Cont0[L, L, V] = r.d match
  case h: Delim.Hand[e, rr, j, t, a, ans] => enter(r, Ev.Entry(h.h, Ev.Unknown[rr]()))
  case _: Delim.Rst[rr, j, t, s, v] => enter(r, Ev.Unknown())

/** what the loop does not take, a `shift` or a raw capture: the rest under the delimiter as a `$` of the kernel, its
 * answers read as values — the delimiter's reading, the same semantics */
private def bail[RB <: Row, L, I0, V, A, T, X](c: Cont0[T, Cont0[L, L, Res[RB, L, V]], X], k: Frames[X, Cont0[L, L, Res[RB, L, I0]], T, A],
    d: Delim[RB, L, I0, V, A]): Cont0[L, L, V] =
  val out: Cont0[L, L, Res[RB, L, V]] = Dollar(Bind(c, k), (a: A) => Return(Res.Done[RB, L, I0](ret(d, a))))
  d match
    case h: Delim.Hand[e, r, j, t, a, ans] => readHand[e, r, j, t, V](out, h.h)
    case _: Delim.Rst[r, j, t, s, v] => passAll[r, j, t, V](out)

// THE DELIMITERS' READING of their answers as values — for what the machine does not take, and for shift0

/** what a delimiter does with an operation reaching it: `again` reads the rest's answers as it reads these */
private[zero] trait Requests[RB <: Row, R <: Row, J, T, V]:
  def apply[E[_], X](op: E[X], in: In[E, RB], k: X => Eff[R, J, T, T, Res[RB, Level[R, J, T], V]],
                     again: Eff[R, J, T, T, Res[RB, Level[R, J, T], V]] => Eff[R, J, T, T, V]): Eff[R, J, T, T, V]

/** a delimiter's reading of its body's answers: its answer, its own; an operation, by `on`; a `shift` up to it —
 * Danvy–Filinski's, the same for every delimiter: the shift's body under this delimiter again, `k` the rest
 * resumed inside it */
private[zero] def read[RB <: Row, R <: Row, J, T, V](out: Eff[R, J, T, T, Res[RB, Level[R, J, T], V]], on: Requests[RB, R, J, T, V])
    : Eff[R, J, T, T, V] =
  Bind(out, (res: Res[RB, Level[R, J, T], V]) => res match
    case Res.Done(v) => Return(v)
    case r: Res.Req[rb, j, v, e, x] => on(r.op, r.in, r.k, read(_, on))
    case c: Res.Ctl[rb, j, v, a, i] => read(again(c), on))

/** a handler's reading: its own by the clause, `k` the rest read again — deep; another's asked again outside, its
 * path one shorter */
private[zero] def readHand[E[_], R <: Row, J, T, Ans](out: Eff[R, J, T, T, Res[E +: R, Level[R, J, T], Ans]],
    h: Handler[[X] =>> Eff[R, J, T, T, X], E, ?, Ans]): Eff[R, J, T, T, Ans] =
  read(out, new Requests[E +: R, R, J, T, Ans]:
    def apply[E2[_], X](op: E2[X], in: In[E2, E +: R], k: X => Eff[R, J, T, T, Res[E +: R, Level[R, J, T], Ans]],
                        again: Eff[R, J, T, T, Res[E +: R, Level[R, J, T], Ans]] => Eff[R, J, T, T, Ans]): Eff[R, J, T, T, Ans] =
      in match
        case In.Here() => h(op, x => again(k(x)))
        case In.There(o) => request[E2, R, J, T, X](op, o).flatMap(x => again(k(x))))

/** a reset's reading: every operation asked again outside — its path the same, the row is — the rest under it again */
private[zero] def passAll[R <: Row, J, T, O](out: Eff[R, J, T, T, Res[R, Level[R, J, T], O]]): Eff[R, J, T, T, O] =
  read(out, new Requests[R, R, J, T, O]:
    def apply[E[_], X](op: E[X], in: In[E, R], k: X => Eff[R, J, T, T, Res[R, Level[R, J, T], O]],
                       again: Eff[R, J, T, T, Res[R, Level[R, J, T], O]] => Eff[R, J, T, T, O]): Eff[R, J, T, T, O] =
      request[E, R, J, T, X](op, in).flatMap(x => again(k(x))))

/** a `shift`'s body under its delimiter again, its value the delimiter's answer, `k` the rest resumed inside */
private def again[RB <: Row, J, V, A, I](c: Res.Ctl[RB, J, V, A, I]): Cont0[J, J, Res[RB, J, V]] =
  Dollar(c.f(a => inside(c.k(a))), (v: V) => Return(Res.Done[RB, J, V](v)))

/** a rest, a program outside, resumed from inside the delimiter: run there, each operation it asks made again
 * inside — its path the same, the row is — and a `shift` up to the rest's own delimiter taken as the delimiter's */
private def inside[RB <: Row, J, V, I](s: Cont0[J, J, Res[RB, J, I]]): Eff[RB, J, V, V, I] =
  Bind(Shift0[J, J, Res[RB, J, V], Level[RB, J, V], Res[RB, J, I]](kk => Bind(s, kk)), (res: Res[RB, J, I]) => res match
    case Res.Done(i) => Return[Level[RB, J, V], I](i)
    case r: Res.Req[rb, j, i, e, x] => Bind(request[e, RB, J, V, x](r.op, r.in), (x: x) => inside[RB, J, V, I](r.k(x)))
    case c: Res.Ctl[rb, j, i, a, i2] => inside[RB, J, V, I](again(c)))
