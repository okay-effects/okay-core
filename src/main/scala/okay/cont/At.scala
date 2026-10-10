package okay.cont

import okay.core.*
import Cont.*

/** THE INSTANCE over `Cont`, the context at the row `R0`, the answer `S0`: effects and control both */
class At[R0 <: Row, S0] extends Effects with Paths.Keep[R0, At[R0, S0]]:
  type M[I, O, A] = Cont[R0, I, O, A]
  type Effs = R0
  type S = S0
  type Within[I, O, A] = Cont[R0, I, O, A]
  type Handling[E[_], I, O, A] = Cont[E +: R0, I, O, A]
  type Handled[E[_], S2] = At[E +: R0, S2]
  type Reset[S2] = At[R0, S2]
  type Answer[S2] = At[R0, S2]
  def pure[S2, A](a: A): Cont[R0, S2, S2, A] = Return(a)
  def delay[I, O, A](m: => Cont[R0, I, O, A]): Cont[R0, I, O, A] = Delay(() => m)
  extension [I, O, A](m: Cont[R0, I, O, A])
    def flatMap[I2, B](f: A => Cont[R0, I2, I, B]): Cont[R0, I2, O, B] = m.flatMap(f)
    def map[B](f: A => B): Cont[R0, I, O, B] = m.map(f)
  def perform[E[_], S2, X](op: E[X], in: In[E, R0]): Cont[R0, S2, S2, X] = Inject(op, in)
  def handle[E[_], A, Ans](h: Handler[[Z] =>> Cont[R0, S0, S0, Z], E, A, Ans])(body: At[E +: R0, Ans] ?=> Cont[E +: R0, Ans, Ans, A]): Cont[R0, S0, S0, Ans] =
    // a node: the body built when the loop enters it — handlers nested by recursion in constant stack
    Fold(() => body(using At()), h.ret, h, Cont.answers(h))
  def intercept[E[_], A, Ans](in: In[E, R0], h: Handler[[Z] =>> Cont[R0, S0, S0, Z], E, A, Ans])(body: At[R0, Ans] ?=> Cont[R0, Ans, Ans, A])
      : Cont[R0, S0, S0, Ans] =
    Icept(() => body(using At()), h.ret, in, h)
  // an `At` itself: `perform` and the rest the one implementation there is, called straight
  type Needing[Es <: AnyKind] = At[R0, S0] & Needs[Es]
  // the path kept in the context itself, no `Paths` between: one load an operation, not two
  def needing[Es <: AnyKind](p: Paths[Es, R0]): At[R0, S0] & Needs[Es] = p.kept(this)
  def one[E0[_]](head: In[E0, R0]): At[R0, S0] & Needs[E0] = NeedyOne(head)
  def row[Es <: Row](sub: Sub[Es, R0]): At[R0, S0] & Needs[Es] = NeedyRow(sub)
  def both[F[_], G[_]](paths: Paths.Both[F, G, R0]): At[R0, S0] & Needs[F + G] = NeedyBoth(paths)
  def answer[S2]: At[R0, S2] = At()
  def top: Top = okay.cont.top
  def shift[I, O, A](f: (A => Cont[R0, O, O, I]) => Cont[R0, O, O, O], same: (I =:= O) | Null): Cont[R0, I, O, A] = Shift(f)
  def reset[S2, O](body: At[R0, S2] ?=> Cont[R0, S2, O, S2]): Cont[R0, S0, S0, O] = Reset(Delay(() => body(using At())), identity)

/** an `At` with the path of the one effect a program needs */
final class NeedyOne[R0 <: Row, S0, E0[_]](head: In[E0, R0]) extends At[R0, S0] with Needs[E0]:
  // `Only`, as in `Paths.One`
  def find[E[_]](r: Req[E, E0]): In[E, R0] = (r: @unchecked) match
    case Req.Only() => head
/** an `At` with the paths of the row of effects a program needs */
final class NeedyRow[R0 <: Row, S0, Es <: Row](sub: Sub[Es, R0]) extends At[R0, S0] with Needs[Es]:
  // `Path`, as in `Paths.Of`
  def find[E[_]](r: Req[E, Es]): In[E, R0] = (r: @unchecked) match
    case Req.Path(in) => sub(in)

/** an `At` with the paths of effects joined a program needs */
final class NeedyBoth[R0 <: Row, S0, F[_], G[_]](paths: Paths.Both[F, G, R0]) extends At[R0, S0] with Needs[F + G]:
  def find[E[_]](r: Req[E, F + G]): In[E, R0] = paths.find(r)

/** `run(body)`: the body at the top over `Cont`, nothing to perform, its answer its value */
def run[A](body: At[Pure, A] ?=> Cont[Pure, A, A, A]): A = body(using At()).value

extension [E[_], T <: Row, A, Ans](body: Cont[E +: T, Ans, Ans, A])
  /** `body.handle(h)`: a handler over the body, a node the loop folds — a value answered by `ret`, an operation of
   * `E` by its clause with the rest of the body under the handler again, lazily, another effect's forwarded; a
   * capture's body in place of the rest, the rest under a delimiter of its own as `k` */
  def handle[Q](h: Handler[[Z] =>> Cont[T, Q, Q, Z], E, A, Ans]): Cont[T, Q, Q, Ans] = Fold(() => body, h.ret, h, Cont.answers(h))

/** `Cont`'s top, for a library re-entering it */
object top extends Top:
  type M[J, I, O, A] = Cont[Pure, I, O, A]
  def at[J]: At[Pure, J] = At()
  def run[J](p: Cont[Pure, J, J, J]): J = p.value
