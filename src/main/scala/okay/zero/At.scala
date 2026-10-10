package okay.zero

// the rows and handlers, okay.core's: imported, not exported — Scala 2's TASTy reader fails on an export of a sealed trait
import okay.core.{Row, +:, +, Pure, In, :?, %, Sub, Req, Paths, Handler, Answering, Value}

import okay.core

/** OKAY.ZERO AS AN INSTANCE of master's `Effects`: a context of `okay.core` whose programs are `Eff` — the
 * programs written over master's `Effects` and `Control`, `okay.std` among them, run on `Cont0`. Its monad is `Eff`
 * at its level `J0`; a delimiter's body is one level in, `Within` and `Handling`, the level `Level[R0, J0, T0]` */
class At[R0 <: Row, J0, T0] private[zero] () extends core.Effects with Paths.Keep[R0, At[R0, J0, T0]]:
  type M[I, O, A] = Eff[R0, J0, I, O, A]
  type Within[I, O, A] = Eff[R0, Level[R0, J0, T0], I, O, A]
  type Handling[E[_], I, O, A] = Eff[E +: R0, Level[R0, J0, T0], I, O, A]
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
  def handle[E[_], A, Ans](h: core.Handler[Prog, E, A, Ans])(body: At[E +: R0, Level[R0, J0, T0], Ans] ?=> Handling[E, Ans, Ans, A])
      : M[T0, T0, Ans] =
    // the body built when the machine enters it (a by-name, kept in the node): delimiters nested by recursion built
    // in constant stack
    handleNode(h, body(using At()))
  def intercept[E[_], A, Ans](in: In[E, R0], h: core.Handler[Prog, E, A, Ans])(body: At[R0, Level[R0, J0, T0], Ans] ?=> Within[Ans, Ans, A])
      : M[T0, T0, Ans] =
    interceptNode(in, h, body(using At()))
  def shift[I, O, A](f: (A => M[O, O, I]) => M[O, O, O], same: (I =:= O) | Null): M[I, O, A] = control(f, same)
  def reset[S2, O](body: At[R0, Level[R0, J0, T0], S2] ?=> Within[S2, O, S2]): M[T0, T0, O] =
    resetNode(body(using At()))

  type Needing[Es <: AnyKind] = At[R0, J0, T0] & core.Needs[Es]
  // the path kept in the context itself, no `Paths` between: one load an operation, not two
  def needing[Es <: AnyKind](p: Paths[Es, R0]): At[R0, J0, T0] & core.Needs[Es] = p.kept(this)
  def one[E0[_]](head: In[E0, R0]): At[R0, J0, T0] & core.Needs[E0] = At.NeedyOne(head)
  def row[Es <: Row](sub: Sub[Es, R0]): At[R0, J0, T0] & core.Needs[Es] = At.NeedyRow(sub)
  def both[F[_], G[_]](paths: Paths.Both[F, G, R0]): At[R0, J0, T0] & core.Needs[F + G] = At.NeedyBoth(paths)

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
  /** a context with the path of the one effect a program needs */
  final class NeedyOne[R0 <: Row, J0, T0, E0[_]](head: In[E0, R0]) extends At[R0, J0, T0] with core.Needs[E0]:
    // `Only`, as in `Paths.One`
    def find[E[_]](r: Req[E, E0]): In[E, R0] = (r: @unchecked) match
      case Req.Only() => head
  /** a context with the paths of the row of effects a program needs */
  final class NeedyRow[R0 <: Row, J0, T0, Es <: Row](sub: Sub[Es, R0]) extends At[R0, J0, T0] with core.Needs[Es]:
    // `Path`, as in `Paths.Of`
    def find[E[_]](r: Req[E, Es]): In[E, R0] = (r: @unchecked) match
      case Req.Path(in) => sub(in)

  /** a context with the paths of effects joined a program needs */
  final class NeedyBoth[R0 <: Row, J0, T0, F[_], G[_]](paths: Paths.Both[F, G, R0]) extends At[R0, J0, T0] with core.Needs[F + G]:
    def find[E[_]](r: Req[E, F + G]): In[E, R0] = paths.find(r)

/** `Cont0`'s top, for a library re-entering it */
object top extends core.Top:
  type M[J, I, O, A] = Eff[Pure, J, I, O, A]
  def at[J]: At[Pure, J, J] = At()
  def run[J](p: Eff[Pure, J, J, J, J]): J = runTop(p)
