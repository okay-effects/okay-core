package okay.zero

import okay.core

/** OKAY.ZERO AS AN INSTANCE of master's `Delimited`: a context of `okay.core` whose programs are `Eff` — the
 * programs written over master's `Effects` and `Control`, `okay.std` among them, run on `Cont0`. Its monad is `Eff`
 * at its level `J0`; a delimiter's body is one level in, `Inner`, the level `Level[R0, J0, T0]` */
class Fx[R0 <: Row, J0, T0] private[zero] () extends core.Delimited:
  type M[X <: Row, I, O, A] = Eff[X, J0, I, O, A]
  type Inner[X <: Row, I, O, A] = Eff[X, Level[R0, J0, T0], I, O, A]
  type R = R0
  type S = T0
  type Handled[E[_], S2] = Fx[E +: R0, Level[R0, J0, T0], S2]
  type Reset[S2] = Fx[R0, Level[R0, J0, T0], S2]
  type Answer[S2] = Fx[R0, J0, S2]
  def answer[S2]: Fx[R0, J0, S2] = Fx()

  def pure[X <: Row, S2, A](a: A): M[X, S2, S2, A] = okay.zero.pure(a)
  def delay[X <: Row, I, O, A](m: => M[X, I, O, A]): M[X, I, O, A] = okay.zero.delay(m)
  extension [X <: Row, I, O, A](m: M[X, I, O, A])
    def flatMap[I2, B](f: A => M[X, I2, I, B]): M[X, I2, O, B] = okay.zero.flatMap(m)(f)
    def map[B](f: A => B): M[X, I, O, B] = okay.zero.map(m)(f)

  def perform[E[_], R2 <: Row, S2, X](op: E[X], in: In[E, R2]): M[R2, S2, S2, X] = request(op, in)
  def handle[E[_], A, Ans](h: core.Handler[M, E, R0, T0, A, Ans])(body: Fx[E +: R0, Level[R0, J0, T0], Ans] ?=> Inner[E +: R0, Ans, Ans, A])
      : M[R0, T0, T0, Ans] =
    handleNode(h, body(using Fx()))
  def shift[I, O, A](f: (A => M[R0, O, O, I]) => M[R0, O, O, O], same: (I =:= O) | Null): M[R0, I, O, A] = control(f, same)
  def reset[S2, O](body: Fx[R0, Level[R0, J0, T0], S2] ?=> Inner[R0, S2, O, S2]): M[R0, T0, T0, O] =
    resetNode(body(using Fx()))

  type Needing[Es <: AnyKind] = Fx[R0, J0, T0] & core.Needs[Es]
  def needing[Es <: AnyKind](p: Paths[Es, R0]): Fx[R0, J0, T0] & core.Needs[Es] = Fx.Needy(p)

object Fx:
  /** `run(body)`: a program written over master's `Effects` and `Control`, at the top, on `Cont0` */
  def run[A](body: Fx[Pure, A, A] ?=> Eff[Pure, A, A, A, A]): A = okay.zero.run(body(using Fx()))

  /** a context with the paths of the effects a program needs */
  final class Needy[R0 <: Row, J0, T0, Es <: AnyKind](p: Paths[Es, R0]) extends Fx[R0, J0, T0] with core.Needs[Es]:
    def find[E[_]](r: Req[E, Es]): In[E, R0] = p.find(r)

/** THE REFERENCE: `Eff` run by the kernel alone — a handler and a reset their shift0 programs, `$`s whose answers
 * are read (`readHand`, `passAll`), an operation and a shift requests to them; no machine, nothing answered in
 * place. Slow, and the definition: what `Fx` runs fast. A shift up to a handler is master's there, `k` the rest
 * under the handler's `ret`, its operations the body's handler's */
class FxRef[R0 <: Row, J0, T0] private[zero] () extends core.Delimited:
  type M[X <: Row, I, O, A] = Eff[X, J0, I, O, A]
  type Inner[X <: Row, I, O, A] = Eff[X, Level[R0, J0, T0], I, O, A]
  type R = R0
  type S = T0
  type Handled[E[_], S2] = FxRef[E +: R0, Level[R0, J0, T0], S2]
  type Reset[S2] = FxRef[R0, Level[R0, J0, T0], S2]
  type Answer[S2] = FxRef[R0, J0, S2]
  def answer[S2]: FxRef[R0, J0, S2] = FxRef()

  def pure[X <: Row, S2, A](a: A): M[X, S2, S2, A] = okay.zero.pure(a)
  def delay[X <: Row, I, O, A](m: => M[X, I, O, A]): M[X, I, O, A] = okay.zero.delay(m)
  extension [X <: Row, I, O, A](m: M[X, I, O, A])
    def flatMap[I2, B](f: A => M[X, I2, I, B]): M[X, I2, O, B] = okay.zero.flatMap(m)(f)
    def map[B](f: A => B): M[X, I, O, B] = okay.zero.map(m)(f)

  def perform[E[_], R2 <: Row, S2, X](op: E[X], in: In[E, R2]): M[R2, S2, S2, X] = request(op, in)
  def handle[E[_], A, Ans](h: core.Handler[M, E, R0, T0, A, Ans])(body: FxRef[E +: R0, Level[R0, J0, T0], Ans] ?=> Inner[E +: R0, Ans, Ans, A])
      : M[R0, T0, T0, Ans] =
    handleRead(h, body(using FxRef()))
  def shift[I, O, A](f: (A => M[R0, O, O, I]) => M[R0, O, O, O], same: (I =:= O) | Null): M[R0, I, O, A] = control(f, null)
  def reset[S2, O](body: FxRef[R0, Level[R0, J0, T0], S2] ?=> Inner[R0, S2, O, S2]): M[R0, T0, T0, O] =
    resetRead(body(using FxRef()))

  type Needing[Es <: AnyKind] = FxRef[R0, J0, T0] & core.Needs[Es]
  def needing[Es <: AnyKind](p: Paths[Es, R0]): FxRef[R0, J0, T0] & core.Needs[Es] = FxRef.Needy(p)

object FxRef:
  /** `run(body)`: a program over master's `Effects` and `Control`, at the top, by the kernel alone */
  def run[A](body: FxRef[Pure, A, A] ?=> Eff[Pure, A, A, A, A]): A = okay.zero.run(body(using FxRef()))

  final class Needy[R0 <: Row, J0, T0, Es <: AnyKind](p: Paths[Es, R0]) extends FxRef[R0, J0, T0] with core.Needs[Es]:
    def find[E[_]](r: Req[E, Es]): In[E, R0] = p.find(r)
