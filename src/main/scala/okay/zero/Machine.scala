package okay.zero

// the rows and handlers, okay.core's: imported, not exported — Scala 2's TASTy reader fails on an export of a sealed trait
import okay.core.{Row, +:, Pure, In, :?, %, Sub, Req, Paths, Handler, Answering, Value}

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

/** the handlers in force inside an interceptor at `in`: those outside, the one at `in` answering nothing in place —
 * its operations go to the interceptor. A walk as long as the path, the row's length at most */
private def masked[E[_], R <: Row](ev: Ev[R], in: In[E, R]): Ev[R] = ev match
  case e: Ev.Entry[e, t] => in match
    case In.Here() => Ev.Entry[e, t](e.outer, null)
    case In.There(out) => Ev.Entry[e, t](masked(e.outer, out), e.answers)
  case _ => ev

/** how deep resumptions run nested in the loops of the shifts' bodies, at most: past it, the stack kept constant */
private inline val Nested = 64

/** how deep the loop runs delimiters and resumptions it enters nested in itself, a JVM frame each — the common,
 * cheapest way: no object pushed. Past it, pushed on the metacontinuation (`Meta`): any depth in constant stack */
private inline val Deep = 32

/** the handler at the path, if it answers in place — `shift0 k. k(v)` is `v`. The first handler's at once; one
 * further out by the path's last lookup, if from these handlers: the same path through the same handlers finds the
 * same — a loop's operations come with one path, so a lookup across any number of handlers is a compare */
private def inPlace[E[_], R <: Row](in: In[E, R], ev: Ev[R]): okay.core.Value[E] | Null = ev match
  case e: Ev.Entry[e, t] => in match
    case In.Here() => e.answers
    case _ =>
      val f = in.found
      if f != null && (f.from eq e) then f.answers
      else
        val v = walk(in, ev)
        in.found = In.Found(e, v)
        v
  case _ => null

@tailrec private def walk[E[_], R <: Row](in: In[E, R], ev: Ev[R]): okay.core.Value[E] | Null = ev match
  case e: Ev.Entry[e, t] => in match
    case In.Here() => e.answers
    case In.There(out) => walk(out, e.outer)
  case _ => null

/** THE DELIMITERS A LOOP IS INSIDE, innermost first — the machine's METACONTINUATION: what takes the program that
 * leaves the innermost delimiter, `Cont0[L, L, V]`, toward the loop's own result `Z`. At the top, the loop's caller; under another delimiter, that
 * delimiter's loop — its frames, its delimiter, the handlers in force there — gone on in the same loop. So a
 * delimiter entered is pushed, not a frame of the JVM: delimiters nested to any depth run in constant stack */
private[zero] enum Meta[L, V, Z]:
  case Top[L, V]() extends Meta[L, V, Cont0[L, L, V]]
  case Under[RB <: Row, L, I0, V, A, X, Z](k: Frames[X, Cont0[L, L, Res[RB, L, I0]], Cont0[L, L, Res[RB, L, V]], A],
      d: Delim[RB, L, I0, V, A], ev: Ev[RB], up: Meta[L, V, Z]) extends Meta[Cont0[L, L, Res[RB, L, V]], X, Z]

/** the loop: `c` with the stack `k` under the delimiter `d`, the handlers in force inside it `ev`, the delimiters
 * outside it `meta` — to the program outside the outermost */
@tailrec private def deliver[RB <: Row, L, I0, V, A, T, X, Z](c: Cont0[T, Cont0[L, L, Res[RB, L, V]], X],
    k: Frames[X, Cont0[L, L, Res[RB, L, I0]], T, A], d: Delim[RB, L, I0, V, A], ev: Ev[RB], n: Int, depth: Int, meta: Meta[L, V, Z]): Z =
  // `out`, the program leaving the delimiter: to the loop's caller, or on in the delimiter outside — the same loop
  inline def leave(out: Cont0[L, L, V]): Z = meta match
    case _: Meta.Top[l, v] => out
    case u: Meta.Under[rb, l, i0, v, a, x, z] => deliver[rb, l, i0, v, a, Cont0[l, l, Res[rb, l, v]], x, Z](out, u.k, u.d, u.ev, n, depth, u.up)
  c match
  case b: Bind[i, t, o, a, bb] => b.m match
    case r: Return[s, aa] => deliver[RB, L, I0, V, A, T, X, Z](b.f(r.a), k, d, ev, n, depth, meta)
    // a handler, a reset, an interceptor bound: entered at once, nested — its value, the usual, bound with no frame
    // pushed; past `Deep` nested, this loop's frames and the bind pushed under it
    case h: Handle[e, r, j, tt, a2, ans] if depth < Deep => enterHandle[e, r, j, tt, a2, ans](h, ev, n, depth + 1) match
      case v: Return[s, vv] => deliver[RB, L, I0, V, A, T, X, Z](b.f(v.a), k, d, ev, n, depth, meta)
      case m => deliver[RB, L, I0, V, A, t, a, Z](m, Frames.Then(b.f, k), d, ev, n, depth, meta)
    case s: Reset[r, j, tt, sv, ov] if depth < Deep => enterReset[r, j, tt, sv, ov](s, ev, n, depth + 1) match
      case v: Return[s2, vv] => deliver[RB, L, I0, V, A, T, X, Z](b.f(v.a), k, d, ev, n, depth, meta)
      case m => deliver[RB, L, I0, V, A, t, a, Z](m, Frames.Then(b.f, k), d, ev, n, depth, meta)
    case ic: Icpt[e, r, j, tt, a2, ans] if depth < Deep => enterIntercept[e, r, j, tt, a2, ans](ic, ev, n, depth + 1) match
      case v: Return[s2, vv] => deliver[RB, L, I0, V, A, T, X, Z](b.f(v.a), k, d, ev, n, depth, meta)
      case m => deliver[RB, L, I0, V, A, t, a, Z](m, Frames.Then(b.f, k), d, ev, n, depth, meta)
    // a clause's k(x) bound, the usual clause: resumed at once, nested, as a handler bound is — its value, the usual,
    // bound with no frame pushed
    case r: Resume[rb, l, i0, v, a2, x, t1, y] if depth < Deep =>
      enter(r, inner[rb, RB, L, V, i0, v, a2](r.d, ev, r.ev), n, depth + 1) match
        case v: Return[s2, vv] => deliver[RB, L, I0, V, A, T, X, Z](b.f(v.a), k, d, ev, n, depth, meta)
        case m => deliver[RB, L, I0, V, A, t, a, Z](m, Frames.Then(b.f, k), d, ev, n, depth, meta)
    case h: Handle[e, r, j, tt, a2, ans] =>
      deliver[e +: r, Cont0[j, j, Res[r, j, tt]], ans, ans, a2, Cont0[Cont0[j, j, Res[r, j, tt]], Cont0[j, j, Res[r, j, tt]], Res[e +: r, Cont0[j, j, Res[r, j, tt]], ans]], a2, Z](h.body, Frames.End(), h, within(h.h, ev), n, depth, Meta.Under[RB, L, I0, V, A, a, Z](Frames.Then(b.f, k), d, ev, meta))
    case s: Reset[r, j, tt, sv, ov] =>
      deliver[r, Cont0[j, j, Res[r, j, tt]], sv, ov, sv, Cont0[Cont0[j, j, Res[r, j, tt]], Cont0[j, j, Res[r, j, tt]], Res[r, Cont0[j, j, Res[r, j, tt]], sv]], sv, Z](s.body, Frames.End(), s, ev, n, depth, Meta.Under[RB, L, I0, V, A, a, Z](Frames.Then(b.f, k), d, ev, meta))
    case ic: Icpt[e, r, j, tt, a2, ans] =>
      deliver[r, Cont0[j, j, Res[r, j, tt]], ans, ans, a2, Cont0[Cont0[j, j, Res[r, j, tt]], Cont0[j, j, Res[r, j, tt]], Res[r, Cont0[j, j, Res[r, j, tt]], ans]], a2, Z](ic.body, Frames.End(), ic, masked(ev, ic.target), n, depth, Meta.Under[RB, L, I0, V, A, a, Z](Frames.Then(b.f, k), d, ev, meta))
    case _ => deliver[RB, L, I0, V, A, t, a, Z](b.m, Frames.Then(b.f, k), d, ev, n, depth, meta)
  case r: Return[s, x] => k match
    case Frames.End() => leave(Return[L, V](ret(d, r.a)))
    case th: Frames.Then[a1, x1, i1, t1, t2, b1] => deliver[RB, L, I0, V, A, t1, x1, Z](th.f(r.a), th.next, d, ev, n, depth, meta)
    case mp: Frames.Mapped[a1, x1, i1, t1, b1] => feed(mp.f(r.a), mp.next) match
      case Fed.Value(b) => leave(Return[L, V](ret(d, b)))
      case nx: Fed.Next[x2, i2, t3, t4, b2] => deliver[RB, L, I0, V, A, t3, x2, Z](nx.c, nx.next, d, ev, n, depth, meta)
  // the handler at the operation's path answering in place, its value at once; not known so, to the delimiter
  case o: OpK[r, j, t, e, x, i, b] => inPlace[e, RB](o.in, ev) match
    case null => leave(route(o.op, o.in, o.k, k, d, ev))
    case v => deliver[RB, L, I0, V, A, T, X, Z](o.k(v.value(o.op)), k, d, ev, n, depth, meta)
  case o: Op[r, j, t, e, x] => inPlace[e, RB](o.in, ev) match
    case null => leave(route(o.op, o.in, unit[T, x], k, d, ev))
    // its value straight into the frame bound to it, the usual — no `Return` made to carry it there
    case v => k match
      case th: Frames.Then[a1, x1, i1, t1, t2, b1] => deliver[RB, L, I0, V, A, t1, x1, Z](th.f(v.value(o.op)), th.next, d, ev, n, depth, meta)
      case _ => deliver[RB, L, I0, V, A, T, X, Z](Return(v.value(o.op)), k, d, ev, n, depth, meta)
  case m: Map[i, o, a, b] => m.m match
    // a clause's k(x) mapped: resumed at once, nested — its value, the usual, mapped with no frame pushed
    case r: Resume[rb, l, i0, v, a2, x, t1, y] if depth < Deep =>
      enter(r, inner[rb, RB, L, V, i0, v, a2](r.d, ev, r.ev), n, depth + 1) match
        case v: Return[s2, vv] => deliver[RB, L, I0, V, A, T, X, Z](Return(m.f(v.a)), k, d, ev, n, depth, meta)
        case mm => deliver[RB, L, I0, V, A, T, a, Z](mm, Frames.Mapped(m.f, k), d, ev, n, depth, meta)
    case _ => deliver[RB, L, I0, V, A, T, a, Z](m.m, Frames.Mapped(m.f, k), d, ev, n, depth, meta)
  case dl: Delay[i, o, a] => deliver[RB, L, I0, V, A, T, X, Z](dl.c(), k, d, ev, n, depth, meta)
  // a resumption, a handler, a reset, an interceptor: entered, nested; past `Deep`, this loop pushed under it
  case r: Resume[rb, l, i0, v, a, x, t1, y] if depth < Deep =>
    deliver[RB, L, I0, V, A, T, X, Z](enter(r, inner[rb, RB, L, V, i0, v, a](r.d, ev, r.ev), n, depth + 1), k, d, ev, n, depth, meta)
  case h: Handle[e, r, j, t, a, ans] if depth < Deep =>
    deliver[RB, L, I0, V, A, T, X, Z](enterHandle[e, r, j, t, a, ans](h, ev, n, depth + 1), k, d, ev, n, depth, meta)
  case s: Reset[r, j, t, sv, o] if depth < Deep =>
    deliver[RB, L, I0, V, A, T, X, Z](enterReset[r, j, t, sv, o](s, ev, n, depth + 1), k, d, ev, n, depth, meta)
  case ic: Icpt[e, r, j, t, a, ans] if depth < Deep =>
    deliver[RB, L, I0, V, A, T, X, Z](enterIntercept[e, r, j, t, a, ans](ic, ev, n, depth + 1), k, d, ev, n, depth, meta)
  case r: Resume[rb, l, i0, v, a, x, t1, y] =>
    deliver[rb, l, i0, v, a, t1, y, Z](r.f(r.x), r.k, r.d, inner[rb, RB, L, V, i0, v, a](r.d, ev, r.ev), n, depth, Meta.Under[RB, L, I0, V, A, X, Z](k, d, ev, meta))
  case h: Handle[e, r, j, t, a, ans] =>
    deliver[e +: r, Cont0[j, j, Res[r, j, t]], ans, ans, a, Cont0[Cont0[j, j, Res[r, j, t]], Cont0[j, j, Res[r, j, t]], Res[e +: r, Cont0[j, j, Res[r, j, t]], ans]], a, Z](h.body, Frames.End(), h, within(h.h, ev), n, depth, Meta.Under[RB, L, I0, V, A, X, Z](k, d, ev, meta))
  case s: Reset[r, j, t, sv, o] =>
    deliver[r, Cont0[j, j, Res[r, j, t]], sv, o, sv, Cont0[Cont0[j, j, Res[r, j, t]], Cont0[j, j, Res[r, j, t]], Res[r, Cont0[j, j, Res[r, j, t]], sv]], sv, Z](s.body, Frames.End(), s, ev, n, depth, Meta.Under[RB, L, I0, V, A, X, Z](k, d, ev, meta))
  case ic: Icpt[e, r, j, t, a, ans] =>
    deliver[r, Cont0[j, j, Res[r, j, t]], ans, ans, a, Cont0[Cont0[j, j, Res[r, j, t]], Cont0[j, j, Res[r, j, t]], Res[r, Cont0[j, j, Res[r, j, t]], ans]], a, Z](ic.body, Frames.End(), ic, masked(ev, ic.target), n, depth, Meta.Under[RB, L, I0, V, A, X, Z](k, d, ev, meta))
  // a program outside run there, its value back into the rest under the delimiter. A shift's k from its body, a
  // resumption: run here, nested, as a delimiter's body is — its value back in this loop, no capture of the rest;
  // anything else, or nested too deep, out and back
  case l: Lift[rb, l, o, x] => l.m match
    case r: Resume[rb2, l2, i02, v2, a2, x2, t12, y2] if n < Nested => d match
      case _: ByReset[r3, j3, t3, s3, v3] =>
        enter(r, inner[rb2, r3, j3, t3, i02, v2, a2](r.d, ev, r.ev), n + 1, depth + 1) match
          case v: Return[s, vv] => deliver[RB, L, I0, V, A, T, X, Z](Return(v.a), k, d, ev, n, depth, meta)
          case m => leave(Bind(m, Resumed(unit[Cont0[L, L, Res[RB, L, V]], X], k, d, ev)))
      case _: ByHandler[e3, r3, j3, t3, a3, i03, v3] =>
        enter(r, inner[rb2, r3, j3, t3, i02, v2, a2](r.d, outside[e3, r3](ev), r.ev), n + 1, depth + 1) match
          case v: Return[s, vv] => deliver[RB, L, I0, V, A, T, X, Z](Return(v.a), k, d, ev, n, depth, meta)
          case m => leave(Bind(m, Resumed(unit[Cont0[L, L, Res[RB, L, V]], X], k, d, ev)))
      // under an interceptor the handlers outside it are not kept: out and back
      case _ => leave(Bind(l.m, Resumed(unit[Cont0[L, L, Res[RB, L, V]], X], k, d, ev)))
    case _ => leave(Bind(l.m, Resumed(unit[Cont0[L, L, Res[RB, L, V]], X], k, d, ev)))
  // a shift up to a delimiter: Danvy–Filinski's, its body under the delimiter again, k the rest under it, lifted in
  // — shift0 and reset0: the body's loop in place of this one, nothing pushed. Up to a handler k's operations are
  // k's handler's, its clauses at its answer: the shift keeps it, `same` — one that moves it is in a reset, by its
  // type; were it not, the reading below
  // a shift0 up to a reset: its body in the reset's place, the program outside — k the rest under the reset again
  case c: Ctl0[r, j, t, a, s, o] => d match
    case _: ByReset[r3, j3, t3, s3, v3] => leave(c.f(x => Resume(x, unit, k, Rst[r3, j3, t3, I0, s](), ev)))
    case _ => leave(bail(c, k, d))
  case c: Ctl[r, j, a, i, o] => d match
    case _: ByReset[r2, j2, t2, s2, v2] =>
      deliver[r2, Cont0[j2, j2, Res[r2, j2, t2]], V, V, V, Cont0[Cont0[j2, j2, Res[r2, j2, t2]], Cont0[j2, j2, Res[r2, j2, t2]], Res[r2, Cont0[j2, j2, Res[r2, j2, t2]], V]], V, Z](
        shiftAtReset[r2, j2, t2, I0, V, a, i](c, k, ev), Frames.End(), Rst[r2, j2, t2, V, V](), ev, n, depth, meta)
    case h: ByHandler[e, r2, j2, t2, a2, i02, v2] => c.same match
      case null => leave(bail(c, k, d))
      case same =>
        deliver[e +: r2, Cont0[j2, j2, Res[r2, j2, t2]], V, V, V, Cont0[Cont0[j2, j2, Res[r2, j2, t2]], Cont0[j2, j2, Res[r2, j2, t2]], Res[e +: r2, Cont0[j2, j2, Res[r2, j2, t2]], V]], V, Z](
          shiftAtHandler[e, r2, j2, t2, I0, V, A, a, i](h.clauses, h.rets, same, c, k, ev), Frames.End(), Hd[e, r2, j2, t2, V, V, V](h.clauses, v => v), ev, n, depth, meta)
    // up to an interceptor, rare: its definition
    case _ => leave(bail(c, k, d))
  case _ => leave(bail(c, k, d))

/** a `shift`'s body up to a reset: its body to run under a reset again, `k` the rest under the reset, lifted in */
private def shiftAtReset[R <: Row, J, T, I0, V, X, Hole](c: Ctl[R, Cont0[J, J, Res[R, J, T]], X, Hole, V],
    k: Frames[X, Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Res[R, Cont0[J, J, Res[R, J, T]], I0]], Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Res[R, Cont0[J, J, Res[R, J, T]], Hole]], I0], ev: Ev[R])
    : Cont0[Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Res[R, Cont0[J, J, Res[R, J, T]], V]], Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Res[R, Cont0[J, J, Res[R, J, T]], V]], V] =
  val rd = Rst[R, J, T, I0, Hole]()
  c.f(x => Lift[R, Cont0[J, J, Res[R, J, T]], V, Hole](Resume(x, unit, k, rd, ev)))

/** a `shift`'s body up to an answering handler: to run under the handler again, `k` the rest under the handler,
 * lifted in */
private def shiftAtHandler[E[_], R <: Row, J, T, I0, V, A, X, Hole](h: Handler[[Y] =>> Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Y], E, ?, V], r: A => I0, same: Hole =:= V,
    c: Ctl[E +: R, Cont0[J, J, Res[R, J, T]], X, Hole, V], k: Frames[X, Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Res[E +: R, Cont0[J, J, Res[R, J, T]], I0]], Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Res[E +: R, Cont0[J, J, Res[R, J, T]], Hole]], A],
    ev: Ev[E +: R]): Cont0[Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Res[E +: R, Cont0[J, J, Res[R, J, T]], V]], Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Res[E +: R, Cont0[J, J, Res[R, J, T]], V]], V] =
  val rd = Hd[E, R, J, T, A, I0, Hole](same.flip.substituteCo[[Z] =>> Handler[[Y] =>> Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Y], E, ?, Z]](h), r)
  c.f(x => Lift[E +: R, Cont0[J, J, Res[R, J, T]], V, Hole](Resume(x, unit, k, rd, ev)))

/** a body's loop from its start, nothing bound under it, the delimiters outside `meta` */
private def deliver0[RB <: Row, L, I0, V, A, Z](c: Cont0[Cont0[L, L, Res[RB, L, I0]], Cont0[L, L, Res[RB, L, V]], A],
    d: Delim[RB, L, I0, V, A], ev: Ev[RB], n: Int, depth: Int, meta: Meta[L, V, Z]): Z =
  deliver[RB, L, I0, V, A, Cont0[L, L, Res[RB, L, I0]], A, Z](c, Frames.End(), d, ev, n, depth, meta)

/** the body's value to the delimiter's answer */
private[zero] def ret[RB <: Row, L, I0, V, A](d: Delim[RB, L, I0, V, A], a: A): I0 = d match
  case h: ByHandler[e, r, j, t, a, i0, v] => h.ret(a)
  case _: ByReset[r, j, t, s, v] => a
  case ic: ByIntercept[e, r, j, t, a, v] => ic.ret(a)

/** an operation not answered in place, at the delimiter: a handler's own — its clause, `k` the rest under it again;
 * another's — on outside, one handler shorter, or as it is past a reset */
private def route[RB <: Row, L, I0, V, A, E[_], X, T1, Y](op: E[X], in: In[E, RB], f: X => Cont0[T1, Cont0[L, L, Res[RB, L, V]], Y],
    k: Frames[Y, Cont0[L, L, Res[RB, L, I0]], T1, A],
    d: Delim[RB, L, I0, V, A], ev: Ev[RB]): Cont0[L, L, V] = d match
  case h: ByHandler[e, r, j, t, a, i0, v] => in match
    case In.Here() => h.clauses(op, Resumed(f, k, d, ev))
    case In.There(o) => OpK[r, j, t, E, X, Cont0[j, j, Res[r, j, t]], v](op, o, Resumed(f, k, d, ev))
  case _: ByReset[r, j, t, s, v] => OpK[r, j, t, E, X, Cont0[j, j, Res[r, j, t]], v](op, in, Resumed(f, k, d, ev))
  // an interceptor's: at its path, its clause, `k` the rest under it again; another's on outside as it is
  case ic: ByIntercept[e, r, j, t, a, v] => In.same(in, ic.target) match
    case null => OpK[r, j, t, E, X, Cont0[j, j, Res[r, j, t]], v](op, in, Resumed(f, k, d, ev))
    case same => ic.clauses(same(op), Resumed(f, k, d, ev))

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
    case ic: ByIntercept[e, r, j, t, a, v] => masked(ev, ic.target)

/** a resumption: its rest under its delimiter again, in a loop of its own */
private[zero] def enter[RB <: Row, L, I0, V, A, X, T1, Y](r: Resume[RB, L, I0, V, A, X, T1, Y], ev: Ev[RB], n: Int, depth: Int): Cont0[L, L, V] =
  // the value through the frames in the loop, no program of the rest built frame by frame
  deliver[RB, L, I0, V, A, T1, Y, Cont0[L, L, V]](r.f(r.x), r.k, r.d, ev, n, depth, r.d.top)

/** a handler entered: its body in a loop of its own, the node its delimiter, the handler in front of those in force */
private[zero] def enterHandle[E[_], R <: Row, J, T, A, Ans](h: Handle[E, R, J, T, A, Ans], ev: Ev[R], n: Int, depth: Int): Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Ans] =
  deliver0(h.body, h, within(h.h, ev), n, depth, h.top)

/** an interceptor entered: its body in a loop of its own, the node its delimiter, the handler at its path answering
 * nothing in place */
private[zero] def enterIntercept[E[_], R <: Row, J, T, A, Ans](ic: Icpt[E, R, J, T, A, Ans], ev: Ev[R], n: Int, depth: Int): Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Ans] =
  deliver0(ic.body, ic, masked(ev, ic.target), n, depth, ic.top)

/** a reset entered: its body in a loop of its own, the node its delimiter */
private[zero] def enterReset[R <: Row, J, T, S, O](s: Reset[R, J, T, S, O], ev: Ev[R], n: Int, depth: Int): Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], O] =
  deliver0(s.body, s, ev, n, depth, s.top)

/** a resumption entered with nothing known of the handlers around it — by the kernel's loop */
private[zero] def enterUnknown[RB <: Row, L, I0, V, A, X, T1, Y](r: Resume[RB, L, I0, V, A, X, T1, Y]): Cont0[L, L, V] = r.d match
  case _: ByHandler[e, rr, j, t, a, i0, v] => enter(r, inner[RB, rr, j, t, I0, V, A](r.d, Ev.Unknown, r.ev), 0, 0)
  case _: ByReset[rr, j, t, s, v] => enter(r, Ev.Unknown, 0, 0)
  case _: ByIntercept[e, rr, j, t, a, v] => enter(r, Ev.Unknown, 0, 0)
