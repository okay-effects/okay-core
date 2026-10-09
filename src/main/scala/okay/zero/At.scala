package okay.zero

// the rows and handlers, okay.core's: imported, not exported — Scala 2's TASTy reader fails on an export of a sealed trait
import okay.core.{Row, +:, Pure, In, :?, %, Sub, Req, Paths, Handler, Answering, Value}

import okay.core

/** OKAY.ZERO AS AN INSTANCE of master's `Effects`: a context of `okay.core` whose programs are `Eff` — the
 * programs written over master's `Effects` and `Control`, `okay.std` among them, run on `Cont0`. Its monad is `Eff`
 * at its level `J0`; a delimiter's body is one level in, `Inner`, the level `Level[R0, J0, T0]` */
class At[R0 <: Row, J0, T0] private[zero] () extends core.Effects:
  type M[I, O, A] = Eff[R0, J0, I, O, A]
  type Inner[X <: Row, I, O, A] = Eff[X, Level[R0, J0, T0], I, O, A]
  type Effs = R0
  type S = T0
  type Handled[E[_], S2] = At[E +: R0, Level[R0, J0, T0], S2]
  type Reset[S2] = At[R0, Level[R0, J0, T0], S2]
  type Answer[S2] = At[R0, J0, S2]
  def answer[S2]: At[R0, J0, S2] = At()
  def top: core.Top = okay.zero.top

  def pure[S2, A](a: A): M[S2, S2, A] = okay.zero.pure(a)
  def delay[I, O, A](m: => M[I, O, A]): M[I, O, A] = okay.zero.delay(m)
  extension [I, O, A](m: M[I, O, A])
    def flatMap[I2, B](f: A => M[I2, I, B]): M[I2, O, B] = okay.zero.flatMap(m)(f)
    def map[B](f: A => B): M[I, O, B] = okay.zero.map(m)(f)

  def perform[E[_], S2, X](op: E[X], in: In[E, R0]): M[S2, S2, X] = request(op, in)
  def handle[E[_], A, Ans](h: core.Handler[Prog, E, A, Ans])(body: At[E +: R0, Level[R0, J0, T0], Ans] ?=> Inner[E +: R0, Ans, Ans, A])
      : M[T0, T0, Ans] =
    handleNode(h, body(using At()))
  def intercept[E[_], A, Ans](in: In[E, R0], h: core.Handler[Prog, E, A, Ans])(body: At[R0, Level[R0, J0, T0], Ans] ?=> Inner[R0, Ans, Ans, A])
      : M[T0, T0, Ans] =
    interceptNode(in, h, body(using At()))
  def shift[I, O, A](f: (A => M[O, O, I]) => M[O, O, O], same: (I =:= O) | Null): M[I, O, A] = control(f, same)
  def reset[S2, O](body: At[R0, Level[R0, J0, T0], S2] ?=> Inner[R0, S2, O, S2]): M[T0, T0, O] =
    resetNode(body(using At()))

  type Needing[Es <: AnyKind] = At[R0, J0, T0] & core.Needs[Es]
  def needing[Es <: AnyKind](p: Paths[Es, R0]): At[R0, J0, T0] & core.Needs[Es] = At.Needy(p)

/** `shift0[A, O](k => body)` up to the nearest reset — `Cont0`'s own, beyond master's: as `shift`, but the body runs
 * OUTSIDE the reset, in its place, in the context outside — so a shift0 in it reaches the next reset out. `k` the
 * rest under the reset again. Its context a reset's of `At`: a handler's row is one longer than its level's, so
 * under a handler it does not compile */
def shift0[A, O]: Shift0To[A, O] = Shift0To()
final class Shift0To[A, O] private[zero] ():
  def apply[R <: Row, J, T, S](using r: At[R, Level[R, J, T], S])(f: At[R, J, T] ?=> (A => Eff[R, J, T, T, S]) => Eff[R, J, T, T, O])
      : Eff[R, Level[R, J, T], S, O, A] =
    shift0Node[R, J, T, A, S, O](f(using At()))

/** `run(body)`: a program written over master's `Effects` and `Control`, at the top, on `Cont0` */
def run[A](body: At[Pure, A, A] ?=> Eff[Pure, A, A, A, A]): A = runTop(body(using At()))

object At:
  /** a context with the paths of the effects a program needs */
  final class Needy[R0 <: Row, J0, T0, Es <: AnyKind](p: Paths[Es, R0]) extends At[R0, J0, T0] with core.Needs[Es]:
    def find[E[_]](r: Req[E, Es]): In[E, R0] = p.find(r)

/** `Cont0`'s top, for a library re-entering it */
object top extends core.Top:
  type M[J, I, O, A] = Eff[Pure, J, I, O, A]
  def at[J]: At[Pure, J, J] = At()
  def run[J](p: Eff[Pure, J, J, J, J]): J = runTop(p)
