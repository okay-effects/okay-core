package okay.zero

import okay.core

/** THE REFERENCE: `Eff` run by the kernel alone — a handler and a reset their shift0 programs, `$`s whose answers
 * are read (`readHand`, `passAll`), an operation and a shift requests to them; no machine, nothing answered in
 * place. Slow, and the definition: what `At` runs fast. A shift up to a handler is master's there, `k` the rest
 * under the handler's `ret`, its operations the body's handler's */
class Ref[R0 <: Row, J0, T0] private[zero] () extends core.Effects:
  type M[I, O, A] = Eff[R0, J0, I, O, A]
  type Inner[X <: Row, I, O, A] = Eff[X, Level[R0, J0, T0], I, O, A]
  type Effs = R0
  type S = T0
  type Handled[E[_], S2] = Ref[E +: R0, Level[R0, J0, T0], S2]
  type Reset[S2] = Ref[R0, Level[R0, J0, T0], S2]
  type Answer[S2] = Ref[R0, J0, S2]
  def answer[S2]: Ref[R0, J0, S2] = Ref()
  def top: core.Top = Ref.top

  def pure[S2, A](a: A): M[S2, S2, A] = okay.zero.pure(a)
  def delay[I, O, A](m: => M[I, O, A]): M[I, O, A] = okay.zero.delay(m)
  extension [I, O, A](m: M[I, O, A])
    def flatMap[I2, B](f: A => M[I2, I, B]): M[I2, O, B] = okay.zero.flatMap(m)(f)
    def map[B](f: A => B): M[I, O, B] = okay.zero.map(m)(f)

  def perform[E[_], S2, X](op: E[X], in: In[E, R0]): M[S2, S2, X] = request(op, in)
  def handle[E[_], A, Ans](h: core.Handler[Prog, E, A, Ans])(body: Ref[E +: R0, Level[R0, J0, T0], Ans] ?=> Inner[E +: R0, Ans, Ans, A])
      : M[T0, T0, Ans] =
    handleRead(h, h.ret, body(using Ref()))
  def shift[I, O, A](f: (A => M[O, O, I]) => M[O, O, O], same: (I =:= O) | Null): M[I, O, A] = control(f, null)
  def reset[S2, O](body: Ref[R0, Level[R0, J0, T0], S2] ?=> Inner[R0, S2, O, S2]): M[T0, T0, O] =
    resetRead(body(using Ref()))

  type Needing[Es <: AnyKind] = Ref[R0, J0, T0] & core.Needs[Es]
  def needing[Es <: AnyKind](p: Paths[Es, R0]): Ref[R0, J0, T0] & core.Needs[Es] = Ref.Needy(p)

object Ref:
  /** `run(body)`: a program over master's `Effects` and `Control`, at the top, by the kernel alone */
  def run[A](body: Ref[Pure, A, A] ?=> Eff[Pure, A, A, A, A]): A = runTop(body(using Ref()))

  final class Needy[R0 <: Row, J0, T0, Es <: AnyKind](p: Paths[Es, R0]) extends Ref[R0, J0, T0] with core.Needs[Es]:
    def find[E[_]](r: Req[E, Es]): In[E, R0] = p.find(r)

  /** the reference's top, for a library re-entering it */
  object top extends core.Top:
    type M[J, I, O, A] = Eff[Pure, J, I, O, A]
    def at[J]: Ref[Pure, J, J] = Ref()
    def run[J](p: Eff[Pure, J, J, J, J]): J = runTop(p)
