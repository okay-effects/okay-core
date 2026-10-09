package okay.zero

import scala.annotation.tailrec
import Cont0.*

// THE MACHINE: a handler's and a reset's body run in a loop of their own, to where it leaves the delimiter — its
// answer by `ret`; an operation answered in place, its handler's by the handlers in force; else at the delimiter, its
// own by the clause, another's passed on outside; a resumption, a handler, a reset entered; a shift, a shift0 taken
// at their delimiter. What it does not take — a raw capture — goes to the delimiters' reading below, the delimiter's
// answers read as values

/** THE HANDLERS IN FORCE around a loop as it runs, innermost first, one per effect of its row — or not known, past a
 * delimiter the machine did not enter: an operation then goes the way it always may, to the delimiters */
private[zero] enum Ev[+R <: Row]:
  case Unknown extends Ev[Nothing]
  /** a handler of `E` in front of the others, `answers` it if it answers in place — found once, when entered */
  case Entry[E[_], T <: Row](outer: Ev[T], answers: okay.core.Value[E] | Null) extends Ev[E +: T]

/** the handlers in force inside a handler `h` entered with `ev` outside: `h` in front — or, if it answers nothing
 * in place and nothing is known outside, nothing known still: a lookup through it finds the same */
private def within[P[_], E[_], R <: Row](h: Handler[P, E, ?, ?], ev: Ev[R]): Ev[E +: R] = h match
  case a: Answering[P, E, ?, ?] => Ev.Entry(ev, a)
  case _ => ev match
    case Ev.Unknown => Ev.Unknown
    case _ => Ev.Entry(ev, null)

/** the handlers in force outside a handler of `E`, those inside it `ev` */
private def outside[E[_], R <: Row](ev: Ev[E +: R]): Ev[R] = ev match
  case e: Ev.Entry[e, t] => e.outer
  case _ => Ev.Unknown

/** how deep resumptions run nested in the loops of the shifts' bodies, at most: past it, the stack kept constant */
private inline val Nested = 64

/** the handler at the path, if it answers in place — `shift0 k. k(v)` is `v` */
@tailrec private def inPlace[E[_], R <: Row](in: In[E, R], ev: Ev[R]): okay.core.Value[E] | Null = ev match
  case e: Ev.Entry[e, t] => in match
    case In.Here() => e.answers
    case In.There(out) => inPlace(out, e.outer)
  case _ => null

/** the loop: `c` with the stack `k` under the delimiter `d`, the handlers in force inside it `ev` — to the program
 * outside the delimiter */
@tailrec private def deliver[RB <: Row, L, I0, V, A, T, X](c: Cont0[T, Cont0[L, L, Res[RB, L, V]], X],
    k: Frames[X, Cont0[L, L, Res[RB, L, I0]], T, A], d: Delim[RB, L, I0, V, A], ev: Ev[RB], n: Int): Cont0[L, L, V] = c match
  case b: Bind[i, t, o, a, bb] => b.m match
    case r: Return[s, aa] => deliver[RB, L, I0, V, A, T, X](b.f(r.a), k, d, ev, n)
    case _ => deliver[RB, L, I0, V, A, t, a](b.m, Frames.Then(b.f, k), d, ev, n)
  case r: Return[s, x] => k match
    case Frames.End() => Return[L, V](ret(d, r.a))
    case th: Frames.Then[a1, x1, i1, t1, t2, b1] => deliver[RB, L, I0, V, A, t1, x1](th.f(r.a), th.next, d, ev, n)
    case mp: Frames.Mapped[a1, x1, i1, t1, b1] => feed(mp.f(r.a), mp.next) match
      case Fed.Value(b) => Return[L, V](ret(d, b))
      case nx: Fed.Next[x2, i2, t3, t4, b2] => deliver[RB, L, I0, V, A, t3, x2](nx.c, nx.next, d, ev, n)
  // the handler at the operation's path answering in place, its value at once; not known so, to the delimiter
  case o: OpK[r, j, t, e, x, i, b] => inPlace[e, RB](o.in, ev) match
    case null => route(o.op, o.in, o.k, k, d, ev)
    case v => deliver[RB, L, I0, V, A, T, X](o.k(v.value(o.op)), k, d, ev, n)
  case o: Op[r, j, t, e, x] => inPlace[e, RB](o.in, ev) match
    case null => route(o.op, o.in, unit[T, x], k, d, ev)
    case v => deliver[RB, L, I0, V, A, T, X](Return(v.value(o.op)), k, d, ev, n)
  case m: Map[i, o, a, b] => deliver[RB, L, I0, V, A, T, a](m.m, Frames.Mapped(m.f, k), d, ev, n)
  case dl: Delay[i, o, a] => deliver[RB, L, I0, V, A, T, X](dl.c(), k, d, ev, n)
  case r: Resume[rb, l, i0, v, a, x, t1, y] => deliver[RB, L, I0, V, A, T, X](enter(r, inner[rb, RB, L, V, i0, v, a](r.d, ev, r.ev), n), k, d, ev, n)
  case h: Handle[e, r, j, t, a, ans] => deliver[RB, L, I0, V, A, T, X](enterHandle[e, r, j, t, a, ans](h, ev, n), k, d, ev, n)
  case s: Reset[r, j, t, sv, o] => deliver[RB, L, I0, V, A, T, X](enterReset[r, j, t, sv, o](s, ev, n), k, d, ev, n)
  // a program outside run there, its value back into the rest under the delimiter. A shift's k from its body, a
  // resumption: run here, nested, as a delimiter's body is — its value back in this loop, no capture of the rest;
  // anything else, or nested too deep, out and back
  case l: Lift[rb, l, o, x] => l.m match
    case r: Resume[rb2, l2, i02, v2, a2, x2, t12, y2] if n < Nested => d match
      case _: ByReset[r3, j3, t3, s3, v3] =>
        enter(r, inner[rb2, r3, j3, t3, i02, v2, a2](r.d, ev, r.ev), n + 1) match
          case v: Return[s, vv] => deliver[RB, L, I0, V, A, T, X](Return(v.a), k, d, ev, n)
          case m => Bind(m, Resumed(unit[Cont0[L, L, Res[RB, L, V]], X], k, d, ev))
      case _: ByHandler[e3, r3, j3, t3, a3, i03, v3] =>
        enter(r, inner[rb2, r3, j3, t3, i02, v2, a2](r.d, outside[e3, r3](ev), r.ev), n + 1) match
          case v: Return[s, vv] => deliver[RB, L, I0, V, A, T, X](Return(v.a), k, d, ev, n)
          case m => Bind(m, Resumed(unit[Cont0[L, L, Res[RB, L, V]], X], k, d, ev))
    case _ => Bind(l.m, Resumed(unit[Cont0[L, L, Res[RB, L, V]], X], k, d, ev))
  // a shift up to a delimiter: Danvy–Filinski's, its body under the delimiter again, k the rest under it, lifted in
  // — shift0 and reset0. Up to a handler k's operations are k's handler's, its clauses at its answer: the shift keeps
  // it, `same` — one that moves it is in a reset, by its type; were it not, the reading below
  // a shift0 up to a reset: its body in the reset's place, the program outside — k the rest under the reset again
  case c: Ctl0[r, j, t, a, s, o] => d match
    case _: ByReset[r3, j3, t3, s3, v3] => c.f(x => Resume(x, unit, k, Rst[r3, j3, t3, I0, s](), ev))
    case _ => bail(c, k, d)
  case c: Ctl[r, j, a, i, o] => d match
    case _: ByReset[r2, j2, t2, s2, v2] => shiftAtReset[r2, j2, t2, I0, V, a, i](c, k, ev, n)
    case h: ByHandler[e, r2, j2, t2, a2, i02, v2] => c.same match
      case null => bail(c, k, d)
      case same => shiftAtHandler[e, r2, j2, t2, I0, V, A, a, i](h.clauses, h.rets, same, c, k, ev, n)
  case _ => bail(c, k, d)

/** a `shift` up to a reset: its body under a reset again, `k` the rest under the reset, run outside, its value back in */
private def shiftAtReset[R <: Row, J, T, I0, V, X, Hole](c: Ctl[R, Cont0[J, J, Res[R, J, T]], X, Hole, V],
    k: Frames[X, Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Res[R, Cont0[J, J, Res[R, J, T]], I0]], Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Res[R, Cont0[J, J, Res[R, J, T]], Hole]], I0], ev: Ev[R], n: Int): Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], V] =
  val rd = Rst[R, J, T, I0, Hole]()
  deliver0[R, Cont0[J, J, Res[R, J, T]], V, V, V](c.f(x => Lift[R, Cont0[J, J, Res[R, J, T]], V, Hole](Resume(x, unit, k, rd, ev))), Rst[R, J, T, V, V](), ev, n)

/** a `shift` up to an answering handler: its body under the handler again, `k` the rest under the handler, run
 * outside, its value back in */
private def shiftAtHandler[E[_], R <: Row, J, T, I0, V, A, X, Hole](h: Handler[[Y] =>> Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Y], E, ?, V], r: A => I0, same: Hole =:= V,
    c: Ctl[E +: R, Cont0[J, J, Res[R, J, T]], X, Hole, V], k: Frames[X, Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Res[E +: R, Cont0[J, J, Res[R, J, T]], I0]], Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Res[E +: R, Cont0[J, J, Res[R, J, T]], Hole]], A],
    ev: Ev[E +: R], n: Int): Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], V] =
  val rd = Hd[E, R, J, T, A, I0, Hole](same.flip.substituteCo[[Z] =>> Handler[[Y] =>> Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Y], E, ?, Z]](h), r)
  deliver0[E +: R, Cont0[J, J, Res[R, J, T]], V, V, V](c.f(x => Lift[E +: R, Cont0[J, J, Res[R, J, T]], V, Hole](Resume(x, unit, k, rd, ev))),
    Hd[E, R, J, T, V, V, V](h, v => v), ev, n)

/** a body's loop from its start, nothing bound under it */
private def deliver0[RB <: Row, L, I0, V, A](c: Cont0[Cont0[L, L, Res[RB, L, I0]], Cont0[L, L, Res[RB, L, V]], A],
    d: Delim[RB, L, I0, V, A], ev: Ev[RB], n: Int): Cont0[L, L, V] =
  deliver[RB, L, I0, V, A, Cont0[L, L, Res[RB, L, I0]], A](c, Frames.End(), d, ev, n)

/** the body's value to the delimiter's answer */
private[zero] def ret[RB <: Row, L, I0, V, A](d: Delim[RB, L, I0, V, A], a: A): I0 = d match
  case h: ByHandler[e, r, j, t, a, i0, v] => h.ret(a)
  case _: ByReset[r, j, t, s, v] => a

/** an operation not answered in place, at the delimiter: a handler's own — its clause, `k` the rest under it again;
 * another's — on outside, one handler shorter, or as it is past a reset */
private def route[RB <: Row, L, I0, V, A, E[_], X, T1, Y](op: E[X], in: In[E, RB], f: X => Cont0[T1, Cont0[L, L, Res[RB, L, V]], Y],
    k: Frames[Y, Cont0[L, L, Res[RB, L, I0]], T1, A],
    d: Delim[RB, L, I0, V, A], ev: Ev[RB]): Cont0[L, L, V] = d match
  case h: ByHandler[e, r, j, t, a, i0, v] => in match
    case In.Here() => h.clauses(op, Resumed(f, k, d, ev))
    case In.There(o) => OpK[r, j, t, E, X, Cont0[j, j, Res[r, j, t]], v](op, o, Resumed(f, k, d, ev))
  case _: ByReset[r, j, t, s, v] => OpK[r, j, t, E, X, Cont0[j, j, Res[r, j, t]], v](op, in, Resumed(f, k, d, ev))

/** a captured rest as `k`: applied, a `Resume` — lazily, so a clause that keeps it runs nothing by making it */
private final class Resumed[RB <: Row, L, I0, V, A, X, T1, Y](f: X => Cont0[T1, Cont0[L, L, Res[RB, L, V]], Y],
    k: Frames[Y, Cont0[L, L, Res[RB, L, I0]], T1, A], d: Delim[RB, L, I0, V, A], ev: Ev[RB]) extends (X => Cont0[L, L, V]):
  def apply(x: X): Cont0[L, L, V] = Resume(x, f, k, d, ev)

/** the rest that is the value itself, before frames: a lambda closing over nothing, one for all */
private def unit[S, A]: A => Cont0[S, S, A] = a => Return(a)

/** the handlers in force inside the delimiter `d`, those outside it `ev`: a handler's in front, a reset's the same —
 * those of when the rest was captured, `was`, if the ones outside are those it had */
private def inner[RB <: Row, RO <: Row, LO, VO, I0, V, A](d: Delim[RB, Cont0[LO, LO, Res[RO, LO, VO]], I0, V, A], ev: Ev[RO], was: Ev[RB]): Ev[RB] =
  d match
    case h: ByHandler[e, r, j, t, a, i0, v] => was match
      case w: Ev.Entry[e2, t2] if w.outer eq ev => was
      case _ => within(h.clauses, ev)
    case _: ByReset[r, j, t, s, v] => ev

/** a resumption: its rest under its delimiter again, in a loop of its own */
private[zero] def enter[RB <: Row, L, I0, V, A, X, T1, Y](r: Resume[RB, L, I0, V, A, X, T1, Y], ev: Ev[RB], n: Int): Cont0[L, L, V] =
  // the value through the frames in the loop, no program of the rest built frame by frame
  deliver[RB, L, I0, V, A, T1, Y](r.f(r.x), r.k, r.d, ev, n)

/** a handler entered: its body in a loop of its own, the node its delimiter, the handler in front of those in force */
private[zero] def enterHandle[E[_], R <: Row, J, T, A, Ans](h: Handle[E, R, J, T, A, Ans], ev: Ev[R], n: Int): Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Ans] =
  deliver0(h.body, h, within(h.h, ev), n)

/** a reset entered: its body in a loop of its own, the node its delimiter */
private[zero] def enterReset[R <: Row, J, T, S, O](s: Reset[R, J, T, S, O], ev: Ev[R], n: Int): Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], O] =
  deliver0(s.body, s, ev, n)

/** a resumption entered with nothing known of the handlers around it — by the kernel's loop */
private[zero] def enterUnknown[RB <: Row, L, I0, V, A, X, T1, Y](r: Resume[RB, L, I0, V, A, X, T1, Y]): Cont0[L, L, V] = r.d match
  case _: ByHandler[e, rr, j, t, a, i0, v] => enter(r, inner[RB, rr, j, t, I0, V, A](r.d, Ev.Unknown, r.ev), 0)
  case _: ByReset[rr, j, t, s, v] => enter(r, Ev.Unknown, 0)
