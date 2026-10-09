package okay.zero

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

// THE PROGRAMS' OPERATIONS, for the contexts of `At` and `Ref` (At.scala): master's interface the way in

extension [R <: Row, J, I, O, A](p: Eff[R, J, I, O, A])
  private[zero] def flatMap[I2, B](f: A => Eff[R, J, I2, I, B]): Eff[R, J, I2, O, B] = p.flatMap(f)
  private[zero] def map[B](f: A => B): Eff[R, J, I, O, B] = p.map(f)

private[zero] def pure[R <: Row, J, T, A](a: A): Eff[R, J, T, T, A] = Return(a)

/** `delay(p)`: the program `p`, built when the loop gets to it — a tail call */
private[zero] def delay[R <: Row, J, I, O, A](p: => Eff[R, J, I, O, A]): Eff[R, J, I, O, A] = Delay(() => p)

/** `run(p)`: the program at the top, under a delimiter of its own — the top's: nothing to ask over `Pure`, a `shift`
 * up to it the program's — its answer its value */
private[zero] def runTop[A](p: Eff[Pure, A, A, A, A]): A =
  // to the head first: its value — no shift up to the top, the common case — or a capture up to the top, the top's
  step(p, Frames.End()) match
    case Head.Done(a) => a
    case c: Head.Cut[i, i1, o1, x1, o, x, a] => top(c.f(x => Dollar(resumed(x, c.k), (a: A) => Return(Res.Done[Pure, A, A](a))))).value

private def top[A](out: Cont0[A, A, Res[Pure, A, A]]): Cont0[A, A, A] =
  Bind(out, (res: Res[Pure, A, A]) => res match
    case Res.Done(a) => Return[A, A](a)
    case c: Res.Ctl[r, j, v, a, i] => top(again(c)))

// THE NODES Control and Effects are written with: an operation, a `shift`, a handler, a reset, a raw capture

/** an operation of `E` at its path, at any answer */
private[zero] def request[E[_], R <: Row, J, T, X](op: E[X], in: In[E, R]): Eff[R, J, T, T, X] = Op[R, J, T, E, X](op, in)

/** a `shift` to the nearest delimiter */
private[zero] def control[R <: Row, J, A, I, O](f: (A => Eff[R, J, O, O, I]) => Eff[R, J, O, O, O], same: (I =:= O) | Null): Eff[R, J, I, O, A] =
  Ctl(f, same)

/** a handler of `E`: its body, its answers read by `h` */
private[zero] def handleNode[E[_], R <: Row, J, T, A, Ans](h: Handler[[X] =>> Eff[R, J, T, T, X], E, A, Ans],
    body: Eff[E +: R, Level[R, J, T], Ans, Ans, A]): Eff[R, J, T, T, Ans] = Handle(body, h)

/** a reset: its body, every operation passed on */
private[zero] def resetNode[R <: Row, J, T, S, O](body: Eff[R, Level[R, J, T], S, O, S]): Eff[R, J, T, T, O] = Reset(body)

/** a `shift0` to the nearest reset, its outside over `R` at `T`: the body in the reset's place, `k` the rest under
 * the reset again */
private[zero] def shift0Node[R <: Row, J, T, A, S, O](f: (A => Eff[R, J, T, T, S]) => Eff[R, J, T, T, O]): Eff[R, Level[R, J, T], S, O, A] =
  Ctl0[R, J, T, A, S, O](f)

/** a `shift0`'s node as the kernel's shift0, where nothing runs it as a reset's: `k` the rest yielding its answers,
 * its operations passed on outside, the body's value the answer */
private[zero] def shift0Program[R <: Row, J, T, A, S, O](c: Ctl0[R, J, T, A, S, O])
    : Cont0[Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Res[R, Cont0[J, J, Res[R, J, T]], S]], Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Res[R, Cont0[J, J, Res[R, J, T]], O]], A] =
  Shift0[Level[R, J, T], Level[R, J, T], Res[R, Level[R, J, T], S], Level[R, Level[R, J, T], O], A](
    k => c.f(a => passAll(k(a))).map(o => Res.Done(o)))

// THE DELIMITERS AS THEIR shift0 PROGRAMS, no machine: a handler, a reset, a `$` whose answers are read — their
// definitions, what the machine runs fast; the reference in the tests (`Ref`) is made of them

/** a handler of `E`: `Done∘h.ret $ body`, its answers read by `h` */
private[zero] def handleRead[E[_], R <: Row, J, T, A, Ans](h: Handler[[X] =>> Eff[R, J, T, T, X], E, A, Ans],
    body: Eff[E +: R, Level[R, J, T], Ans, Ans, A]): Eff[R, J, T, T, Ans] =
  readHand[E, R, J, T, Ans](Dollar(body, (a: A) => Return(Res.Done[E +: R, Level[R, J, T], Ans](h.ret(a)))), h)

/** a reset: `Done $ body`, every operation passed on */
private[zero] def resetRead[R <: Row, J, T, S, O](body: Eff[R, Level[R, J, T], S, O, S]): Eff[R, J, T, T, O] =
  passAll[R, J, T, O](Dollar(body, (s: S) => Return(Res.Done[R, Level[R, J, T], S](s))))

// THE BRIDGE from the machine (Machine.scala) to the reading below

/** what the loop does not take, a raw capture: the rest under the delimiter as a `$` of the kernel, its
 * answers read as values — the delimiter's reading, the same semantics */
private[zero] def bail[RB <: Row, L, I0, V, A, T, X](c: Cont0[T, Cont0[L, L, Res[RB, L, V]], X], k: Frames[X, Cont0[L, L, Res[RB, L, I0]], T, A],
    d: Delim[RB, L, I0, V, A]): Cont0[L, L, V] =
  val out: Cont0[L, L, Res[RB, L, V]] = Dollar(Bind(c, k), (a: A) => Return(Res.Done[RB, L, I0](ret(d, a))))
  d match
    case h: ByHandler[e, r, j, t, a, i0, v] => readHand[e, r, j, t, V](out, h.clauses)
    case _: ByReset[r, j, t, s, v] => passAll[r, j, t, V](out)

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
